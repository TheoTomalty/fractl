#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <errno.h>
#include <stdbool.h>
#include <poll.h>
#include <string.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/stat.h>


#define passert(form, s)                        \
    ({                                          \
        if ((form) < 0) {                       \
            perror(s);                          \
            exit(EXIT_FAILURE);                 \
        }                                       \
    })

#define error(...)                              \
    ({ fprintf(stderr, "fsh: " __VA_ARGS__);    \
        exit(EXIT_FAILURE);                     \
    })


extern int sigfd[2];

static bool fd_read_pending(int fd) {
    struct pollfd pfd = {0};
    pfd.fd = fd;
    pfd.events = POLLIN;
    int ret;
    do { ret = poll(&pfd, 1, 0); } while (ret < 0 && errno == EINTR);
    return ret == 1;
}

static int ptr_array_size(void **p) {
    int ret = 0;
    while (*p != NULL) {
        ++p; ++ret;
    }
    return ret;
}

static int sigchldfd[2] = { -1, -1 };

int write_sigchld(void) {
    char zero = 0;
    int ret = write(sigchldfd[1], &zero, 1);
    if (ret < 0 && errno == EBADF) {
        return -1;
    }
    passert(ret, "write");
    return 0;
}

#define STATUS_RUNNING 0b100
#define STATUS_STOPPED 0b010
#define STATUS_DONE 0b001

struct status {
    pid_t pid;
    bool wstatus_set;
    int wstatus;
};

static struct status **jobs = NULL;
static int jobs_len = 0;
static int jobs_capacity = 0;

void init_shell(void) {
    umask(022);
    if (jobs == NULL) {
        jobs = calloc(16, sizeof(*jobs));
        jobs_len = 0;
        jobs_capacity = 16;
    }
}

static int push_job(struct status *job) {
    int len = jobs_len++;
    if (jobs_capacity < len + 1) {
        struct status **old_jobs = jobs;
        jobs_capacity = jobs_capacity * 2;
        jobs = calloc(jobs_capacity, sizeof(*jobs));
        memcpy(jobs, old_jobs, sizeof(*jobs) * len);
        free(old_jobs);
    }
    jobs[len] = job;
    return len;
}

static int make_job(int n) {
    struct status *ret = calloc(n + 1, sizeof(*ret));
    return push_job(ret);
}

int job_remove(int n) {
    if (jobs_len <= n)
        return -1;
    free(jobs[n]);
    for (int i = n + 1; i < jobs_len; ++i) {
        jobs[i - 1] = jobs[i];
    }
    jobs[jobs_len--] = NULL;
    return 0;
}

int jobs_length(void) {
    return jobs_len;
}

int job_pgid(int n) {
    if (jobs_len <= n)
        return -1;
    return jobs[n]->pid;
}

int job_status(int n) {
    if (jobs_len <= n)
        return -1;
    int status = 0;
    for (struct status *p = jobs[n]; p->pid != 0; ++p) {
        if (p->wstatus_set) {
            if (WIFEXITED(p->wstatus) || WIFSIGNALED(p->wstatus)) {
                status |= STATUS_DONE;
            }
            else if (WIFSTOPPED(p->wstatus)) {
                status |= STATUS_STOPPED;
            }
        }
        else {
            status |= STATUS_RUNNING;
        }
    }
    return status;
}

int job_exit_code(int n) {
    if (jobs_len <= n)
        return -1;
    int ret = -1;
    for (struct status *p = jobs[n]; p->pid != 0; ++p) {
        if (p->wstatus_set) {
            if (WIFEXITED(p->wstatus)) {
                ret = WEXITSTATUS(p->wstatus);
            }
            else if (WIFSIGNALED(p->wstatus)) {
                ret = 128 + WTERMSIG(p->wstatus);
            }
            else if (WIFSTOPPED(p->wstatus)) {
                ret = 128 + WSTOPSIG(p->wstatus);
            }
        }
    }
    return ret;
}

void handle_sigchld(void) {
    int wstatus;
    pid_t pid;
    while ((pid = waitpid(-1, &wstatus, WNOHANG | WUNTRACED | WCONTINUED)) != 0 && errno != ECHILD) {
        passert(pid, "waitpid");
        for (int i = 0; i < jobs_len; i++) {
            for (struct status *p = jobs[i]; p->pid != 0; ++p) {
                if (p->pid == pid) {
                    p->wstatus_set = true;
                    p->wstatus = wstatus;
                }
            }
        }
    }
}

void hangup(void) {
    for (int i = 0; i < jobs_len; i++) {
        pid_t pgid = job_pgid(i);
        kill(-pgid, SIGHUP);
    }
}

struct redirect {
    enum redirect_type {
        REDIRECT_INPUT,
        REDIRECT_OUTPUT,
        REDIRECT_OUTPUT_CLOBBER,
        REDIRECT_OUTPUT_APPEND,
        REDIRECT_INPUT_OUTPUT,
        REDIRECT_INPUT_COPY,
        REDIRECT_OUTPUT_COPY,
        REDIRECT_COPY,
        REDIRECT_MAX,
    } type;
    int fd;
    char *word;
};

struct command {
    char *pathname;
    char **argv;
    char **envp;
    struct redirect **redirects;
};

struct redirect *make_redirect(int type, int fd, char *word) {
    if (type < 0 || type >= REDIRECT_MAX || fd < -1 || word == NULL) return NULL;
    struct redirect *ret = malloc(sizeof(*ret));
    ret->type = type;
    ret->fd = fd;
    ret->word = word;
    return ret;
}

struct command *make_command(char *pathname, char **argv, char **envp, struct redirect **redirects) {
    struct command *ret = calloc(1, sizeof(*ret));
    ret->pathname = pathname;
    ret->argv = argv;
    ret->envp = envp;
    ret->redirects = redirects;
    return ret;
}

void free_redirect(struct redirect *r) {
    free(r->word);
    free(r);
}

static void free_command(struct command *c) {
    free(c->pathname);
    for (int i = 0; c->argv[i] != NULL; ++i) {
        free(c->argv[i]);
    }
    for (int i = 0; c->envp[i] != NULL; ++i) {
        free(c->envp[i]);
    }
    for (int i = 0; c->redirects[i] != NULL; ++i) {
        free_redirect(c->redirects[i]);
    }
    free(c->argv);
    free(c->envp);
}

static void free_pipeline(struct command **p) {
    int size = ptr_array_size((void **) p);
    for (int i = 0; i < size; ++i) {
        free_command(p[i]);
    }
    free(p);
}

static void reset_signals(void) {
    // TODO: do we need to do this?
    sigset_t sigset;
    passert(sigfillset(&sigset), "sigfillset");
    passert(sigprocmask(SIG_UNBLOCK, &sigset, NULL), "sigprocmask");
    
    for (int i = 1; i < NSIG; ++i) {
        if (i != SIGKILL && i != SIGSTOP) {
            signal(i, SIG_DFL);
        }
    }
}

// TODO: implement fg and bg (%+ and %-)

static void foreground_pgrp(void) {
    pid_t pgid;
    passert(pgid = getpgid(0), "getpid");

    // Need to ignore SIGTTOU, otherwise process is stopped
    sigset_t new_set, old_set;
    passert(sigemptyset(&new_set), "sigemptyset");
    passert(sigaddset(&new_set, SIGTTOU), "sigaddset");
    passert(sigprocmask(SIG_BLOCK, &new_set, &old_set), "sigprocmask");
    passert(tcsetpgrp(STDIN_FILENO, pgid), "tcsetpgrp");
    passert(sigprocmask(SIG_SETMASK, &old_set, NULL), "sigprocmask");
}

static int parse_copy_target(char *s) {
    // Returns positive int for file descriptor, -1 for "-", or -2 for
    // invalid target.
    if ('\0' == s[0]) return -2;
    if ('-' == s[0] && '\0' == s[1]) return -1;
    int fd = 0;
    int i = 0;
    for (; '0' <= s[i] && s[i] <= '9'; ++i) {
        fd = fd * 10 + (s[i] - '0');
    }
    if (s[i] == '\0' && i != 0) return fd;
    return -2;
}

int apply_redirect(struct redirect *r, int keep) {
    // If keep is set, return a copy of the old file descriptor.
    // Returns:
    // -1) for normal syscall error
    // -2) for parse copy target error
    // -3) for invalid target of *_COPY redirects
    int ret, tmpfd, flags, fd, kept_fd;

    if (keep) {
        ret = dup(r->fd);
        if (ret < 0) return ret;
        kept_fd = ret;
    }

    switch (r->type) {
    case REDIRECT_INPUT:
        flags = O_RDONLY;
        goto standard;
    case REDIRECT_OUTPUT:
    case REDIRECT_OUTPUT_CLOBBER:
        flags = O_WRONLY | O_CREAT;
        goto standard;
    case REDIRECT_OUTPUT_APPEND:
        flags = O_WRONLY | O_CREAT | O_APPEND;
        goto standard;
    case REDIRECT_INPUT_OUTPUT:
        flags = O_RDWR | O_CREAT;
        goto standard;
    standard:
        ret = open(r->word, flags, 0666);
        if (ret >= 0) {
            tmpfd = ret;
            ret = dup2(tmpfd, r->fd);
            close(tmpfd);
        }
        break;
    case REDIRECT_INPUT_COPY:
        flags = O_RDONLY;
        goto copy;
    case REDIRECT_OUTPUT_COPY:
        flags = O_WRONLY;
        goto copy;
    case REDIRECT_COPY:
        flags = -1;
        goto copy;
    copy:
        ret = parse_copy_target(r->word);
        if (ret >= 0) {
            tmpfd = ret;
            ret = fcntl(tmpfd, F_GETFL);
            if (ret >= 0) {
                if (flags == -1 || (ret & 0b11) == flags || (ret & 0b11) == O_RDWR) {
                    ret = dup2(tmpfd, r->fd);
                }
                else {
                    ret = -3;
                }
            }
            else if (errno == EBADF) {
                ret = -3;
            }
        }
        else if (ret == -1) {
            close(r->fd);
            ret = 0;
        }
        break;
    default:
        error("unreachable");
    }

    if (keep) {
        if (ret < 0) {
            close(kept_fd);
            return ret;
        }
        else return kept_fd;
    }
    else {
        if (ret < 0) return ret;
        else return 0;
    }
}

static void apply_redirect_strict(struct redirect *r) {
    int ret = apply_redirect(r, false);
    if (ret == -2) {
        error("'%s' is not a valid file descriptor\n", r->word);
    }
    else if (ret == -3) {
        error("fd %s is not open for %s\n", r->word, r->type == REDIRECT_INPUT_COPY ? "reading" : "writing");
    }
    passert(ret, "fsh");
}

static pid_t run_command(struct command *c, int wait, pid_t pgid, int in_fd, int out_fd) {
    pid_t pid;
    passert(pid = fork(), "fork");
    if (pid == 0) {
        // Set this process as its own process group leader and take
        // control of the terminal before any I/O takes place.
        passert(setpgid(0, pgid), "setpgid");
        if (wait) foreground_pgrp();

        // Set all signal handlers to defaults for the new process
        reset_signals();

        // Setup file descriptors
        if (sigfd[0] >= 0) close(sigfd[0]);
        if (sigfd[1] >= 0) close(sigfd[1]);
        if (in_fd != -1) {
            passert(dup2(in_fd, STDIN_FILENO), "dup2");
            passert(close(in_fd), "close");
        }
        if (out_fd != -1) {
            passert(dup2(out_fd, STDOUT_FILENO), "dup2");
            passert(close(out_fd), "close");
        }

        for (int i = 0; c->redirects[i] != NULL; ++i) {
            apply_redirect_strict(c->redirects[i]);
        }

        int environ_len = ptr_array_size((void **) environ),
            envp_len = ptr_array_size((void **) c->envp);
        char **envp = malloc((environ_len + envp_len + 1) * sizeof(char *));
        envp[environ_len + envp_len] = NULL;
        memcpy(&envp[0], c->envp, envp_len * sizeof(char *));
        memcpy(&envp[envp_len], environ, environ_len * sizeof(char *));
        passert(execve(c->pathname, c->argv, envp), "execve");
        error("unreachable");
    }
    return pid;
}

int run_pipeline(struct command **p, int wait) {
    int size = ptr_array_size((void **) p);
    
    // Block new signals until after the fork so that the child
    // process does not execute any host code before execve.
    sigset_t new_set;
    sigset_t old_set;
    passert(sigfillset(&new_set), "sigfillset");
    passert(sigprocmask(SIG_BLOCK, &new_set, &old_set), "sigprocmask");

    int job = make_job(size);

    pid_t pgid = 0;
    int fds[2] = { -1, -1 };
    for (int i = 0; i < size; ++i) {
        int old_read = fds[0];
        if (i < size -1) {
            passert(pipe2(fds, O_CLOEXEC), "pipe2");
        }
        else {
            fds[1] = -1; fds[0] = -1;
        }
        pid_t pid = run_command(p[i], wait, pgid, old_read, fds[1]);
        if (old_read != -1) passert(close(old_read), "close");
        if (fds[1] != -1) passert(close(fds[1]), "close");
        
        // Set pgid of leader in parent process too so that this process
        // group is guaranteed to exist for the other commands in the
        // pipeline.
        if (pgid == 0) {
            passert(setpgid(pid, pid), "setpgid");
            pgid = pid;
        }

        // Register the child process in jobs before signalling is
        // restored.
        jobs[job][i].pid = pid;
    }
    free_pipeline(p);

    if (wait) {
        // Register the fd used to accept SIGCHLD signals before signals
        // are unblocked so that changes to forked process are not missed.
        passert(pipe2(sigchldfd, O_CLOEXEC), "pipe2");
    }

    passert(sigprocmask(SIG_SETMASK, &old_set, NULL), "sigprocmask");
    int ret = 0;
    if (wait) {
        int readfd = sigchldfd[0], writefd = sigchldfd[1];
        char byte;
        // Wait for changes to the child process, specifically.
        while (job_status(job) & STATUS_RUNNING) {
            passert(read(readfd, &byte, 1), "read");
            handle_sigchld();
        }
        ret = job_exit_code(job);
        if (job_status(job) == STATUS_DONE) {
            job_remove(job);
        }

        // Restore shell to controlling terminal as soon as possible after
        // child exits.
        foreground_pgrp();

        // Disable the SIGCHLD fd so that signals are forwarded to the
        // normal input system.
        sigchldfd[1] = -1;
        sigchldfd[0] = -1;

        // No longer possible to get new SIGCHLD on readfd, all that is
        // left to do is handle the signals that may already be pending.
        while (fd_read_pending(readfd)) {
            passert(read(readfd, &byte, 1), "read");
            handle_sigchld();
        }
        close(readfd);
        close(writefd);
    }

    return ret;
}
