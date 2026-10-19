#define _GNU_SOURCE
#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/signalfd.h>
#include <poll.h>
#include <errno.h>
#include <stdbool.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#define passert(form)                           \
    ({                                          \
        if ((form) < 0) {                       \
            perror(#form);                      \
            exit(EXIT_FAILURE);                 \
        }                                       \
    })

#define C_EXT 0xE000
#define C_EXT_MASK 0xFFF

/* First 128 chars are unmapped (but correspond to the normal ASCII chars) */
#define C_UP_ARROW    (C_EXT | 0x80)
#define C_DOWN_ARROW  (C_EXT | 0x81)
#define C_RIGHT_ARROW (C_EXT | 0x82)
#define C_LEFT_ARROW  (C_EXT | 0x83)
#define C_SCROLL_UP   (C_EXT | 0x84)
#define C_SCROLL_DOWN (C_EXT | 0x85)
#define C_PASTE       (C_EXT | 0x86)
#define C_UNPASTE     (C_EXT | 0x87)

#define C_META (C_EXT | 0x100)
#define C_META_MASK 0xFF

#define C_SIGNAL (C_EXT | 0x200)
#define C_SIGNAL_MASK 0x3F

#define CHUNK_SIZE 256

struct chunk {
    struct chunk *next;
    int len;
    char buffer[CHUNK_SIZE];
};

static struct termios default_termios;
static struct termios editor_termios;
static struct termios *current_termios;

static void align_chunk(struct chunk **chunk, int *offset) {
    while ((*chunk) != NULL && (*chunk)->len == *offset) {
        *chunk = (*chunk)->next;
        *offset = 0;
    }
}

static signed char next_char(struct chunk **chunk, int *offset) {
    align_chunk(chunk, offset);
    if (*chunk == NULL) return -1;
    signed char ch = (*chunk)->buffer[(*offset)++];
    align_chunk(chunk, offset);
    assert(ch >= 0);
    return ch;
}

static signed char peek_char(struct chunk **chunk, int *offset) {
    struct chunk *local_chunk = *chunk;
    int local_offset = *offset;
    return next_char(&local_chunk, &local_offset);
}

static int parse_digits(struct chunk **chunk, int *offset) {
    int n = 0;
    int i = 0;
    signed char ch;
    while ('0' <= (ch = peek_char(chunk, offset)) && ch <= '9') {
        next_char(chunk, offset);
        n = (n * 10) + (ch - '0');
        i++;
    }
    if (i == 0) return -1;
    else return n;
}

static int mouse_x = 0;
static int mouse_y = 0;
static int report_x = -1;
static int report_y = -1;
static char *pasted_text = NULL;

static int _parse_chunk(struct chunk **chunk, int *offset) {
    // Parse sequence for escape codes and return char or
    // -1) if the escape sequence parsing failed or there is no more input
    // -2) if the sequence is to be ignored.
    // Updates chunk and offset addresses regardless of
    // success/failure.
    enum p_state {
        P_BEGIN,
        P_ESCAPE,
        P_CSI,
    } state = P_BEGIN;
    signed char ch, endch;
    int d1, d2, d3, ret;

    while (true) {
        ch = next_char(chunk, offset);
        switch (state) {
        case P_BEGIN:
            if (ch == '\e' && 0 <= peek_char(chunk, offset)) {
                state = P_ESCAPE;
                break;
            }
            else return ch;
        case P_ESCAPE:
            if (ch == '[') {
                if (0 <= (ch = peek_char(chunk, offset))) {
                    if ((d1 = parse_digits(chunk, offset)) >= 0) {
                        switch (next_char(chunk, offset)) {
                        case ';': goto report;
                        case '~':
                            switch (d1) {
                            case 200: return C_PASTE;
                            case 201: return C_UNPASTE;
                            default: return -1;
                            }
                        default: return -1;
                        }
                        report:
                        if ((d2 = parse_digits(chunk, offset)) < 0) return -1;
                        if ('R' != next_char(chunk, offset)) return -1;
                        report_x = d2 - 1;
                        report_y = d1 - 1;
                        return -2;
                    }
                    else {
                        state = P_CSI;
                        break;
                    }
                }
                else return C_META | ('[' & C_META_MASK);
            }
            else return C_META | (ch & C_META_MASK);
        case P_CSI:
            switch (ch) {
            case '<': {
                if ((d1 = parse_digits(chunk, offset)) < 0) return -1;
                int move = d1 & 0x20, button = d1 & 0x1f, scroll = d1 & 0xc0;
                if (';' != next_char(chunk, offset)) return -1;
                if ((d2 = parse_digits(chunk, offset)) < 0) return -1;
                if (';' != next_char(chunk, offset)) return -1;
                if ((d3 = parse_digits(chunk, offset)) < 0) return -1;
                if ('M' != (ch = next_char(chunk, offset)) && 'm' != ch) return -1;

                mouse_x = d2; mouse_y = d3;
                if (move) return -2;
                if (scroll) {
                    switch (button) {
                    case 0: return C_SCROLL_UP;
                    case 1: return C_SCROLL_DOWN;
                    default: return -2;
                    }
                }
                return -2;
            }
            case 'A': return C_UP_ARROW;
            case 'B': return C_DOWN_ARROW;
            case 'C': return C_RIGHT_ARROW;
            case 'D': return C_LEFT_ARROW;
            default: return -1;
            }
        }
    }
}

static int parse_chunk(struct chunk **chunk, int *offset) {
    // Parse sequence for escape codes and return char or
    // -1) if there is no more input
    // -2) if the sequence is to be ignored.
    struct chunk *local_chunk = *chunk;
    int local_offset = *offset;
    int ch = _parse_chunk(&local_chunk, &local_offset);
    if (ch == -1) {
        return next_char(chunk, offset);
    }
    *chunk = local_chunk;
    *offset = local_offset;
    return ch;
}

int infd = -1;
int sigfd[2] = { -1, -1 };

int termio_initialised_p() {
    return infd != -1;
}

void init_termio(int input_fd) {
    passert(tcgetattr(input_fd, &default_termios));

    memcpy(&editor_termios, &default_termios, sizeof(struct termios));
    editor_termios.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | INPCK | ISTRIP |
                                INLCR  | IGNCR  | ICRNL  | IUCLC | IXON   |
                                IXANY  | IXOFF);
    editor_termios.c_oflag &= ~(OPOST  | OLCUC  | ONLCR  | OCRNL | ONOCR  |
                                ONLRET | OFILL  | OFDEL);
    editor_termios.c_lflag &= ~(ISIG   | ICANON | XCASE  | ECHO  | IEXTEN);
    editor_termios.c_lflag &= TOSTOP;
    editor_termios.c_cflag &= ~(CSIZE  | PARENB);
    editor_termios.c_cflag |= CS8;

    infd = input_fd;
    passert(pipe2(sigfd, O_CLOEXEC));
}

void write_signal(int signal) {
    passert(write(sigfd[1], &signal, sizeof(signal)));
}

static int read_signal() {
    int signal;
    passert(read(sigfd[0], &signal, sizeof(signal)));
    return signal;
}

static void activate_termios(struct termios *termios) {
    if (current_termios != termios) {
        if (tcsetattr(infd, TCSANOW, termios) < 0) {
            perror("tcsetattr");
            exit(1);
        }
        current_termios = termios;
    }
}

void activate_default_termios() {
    activate_termios(&default_termios);
}

void activate_editor_termios() {
    activate_termios(&editor_termios);
}

static void check_pollfd(struct pollfd *fd) {
    if (fd->revents & (POLLERR | POLLHUP | POLLNVAL)) {
        fprintf(stderr, "poll error\n");
        exit(1);
    }
}

static bool fd_read_pending(int fd) {
    struct pollfd pfd = {0};
    pfd.fd = fd;
    pfd.events = POLLIN;
    int ret;
    do { ret = poll(&pfd, 1, 0); } while (ret < 0 && errno == EINTR);
    return ret == 1;
}

static struct chunk *pending_chunk = NULL;
static int pending_offset = 0;
static int pending_reports_ignored = 0;

static void handle_read(int fd, struct chunk **loc) {
    struct chunk **last = loc;
    do {
        struct chunk *next = malloc(sizeof(**loc));
        next->next = *last;
        *last = next;
        last = &next->next;
        do { next->len = read(fd, &next->buffer, sizeof(next->buffer)); }
        while (next->len < 0 && errno == EINTR);
        if (next->len < 0) { perror("read"); exit(1); }
    }
    while (fd_read_pending(fd));
}

static int handle_signal(int fd) {
    int n, signal;
    passert(n = read(fd, &signal, sizeof(signal)));
    assert(n == sizeof(signal));
    return C_SIGNAL | (signal & C_SIGNAL_MASK);
}

int read_input(int timeout) {
    /* Input is read in chunks, rather than one character at a time
       (to properly differentiate between escape sequences and user
       input involving the escape key).  If there is pending input,
       parse the next character from that.  Otherwise, listen for more
       input from the user, or signals on the signalfd.  If no event
       occurs before timeout (-1 for infinite wait) then return -1.
     */
    while (pending_chunk != NULL) {
        struct chunk *chunk = pending_chunk;
        int offset = pending_offset;
        report_x = -1; report_y = -1;
        int ch = parse_chunk(&chunk, &offset);
        if (report_x != -1) pending_reports_ignored--;
        while (pending_chunk != chunk) {
            struct chunk *next = pending_chunk->next;
            free(pending_chunk);
            pending_chunk = next;
        }
        pending_offset = offset;
        if (ch >= 0) return ch;
    }
    
    struct pollfd fds[2] = {0};
    fds[0].fd = sigfd[0];
    fds[0].events = POLLIN;
    fds[1].fd = infd;
    fds[1].events = POLLIN;

    int n;
    do { n = poll(fds, 2, timeout); } while (n < 0 && errno == EINTR);
    check_pollfd(&fds[0]);
    check_pollfd(&fds[1]);
    if (n == 0) {
        return -1;
    }
    if (fds[0].revents) {
        return handle_signal(fds[0].fd);
    }
    if (fds[1].revents) {
        handle_read(fds[1].fd, &pending_chunk);
        pending_offset = 0;
        // TODO: the timeout should be reduced by the differential since calling poll
        return read_input(timeout);
    }
}

static struct chunk **pending_chunk_end() {
    struct chunk **ret = &pending_chunk;
    while (*ret != NULL) {
        ret = &(*ret)->next;
    }
    return ret;
}

unsigned int read_cursor_pos() {
    /* Get input until there is one more report sequence (do not
       consume input).  Ignores previous report sequences that have
       been requested in the current pending input.
     */
    struct chunk *chunk = pending_chunk;
    int offset = pending_offset;
    int n = (pending_reports_ignored++) + 1;
    while (n-- > 0) {
        report_x = -1; report_y = -1;
        while (report_x == -1) {
            if (parse_chunk(&chunk, &offset) == -1) {
                struct chunk **loc = pending_chunk_end();
                handle_read(infd, loc);
                chunk = *loc;
                offset = 0;
            }
        }
    }
    return ((unsigned int) (report_x & 0xFFFF)) | (((unsigned int) (report_y & 0xFFFF)) << ((unsigned int) 16));
}

struct termsize {
    int x;
    int y;
};

void termsize(struct termsize *size) {
    struct winsize winsize;
    ioctl(infd, TIOCGWINSZ, &winsize);
    size->x = winsize.ws_col;
    size->y = winsize.ws_row;
}

/* void main() { */
/*     init_termio(0); */
/*     activate_editor_termios(); */
/*     while (true) { */
/*         int ch = read_input(); */
/*         if (ch == 3) { activate_default_termios(); return; } */
/*         printf("#x%x\n\r", ch); */
/*         fflush(stdout); */
/*     } */
/* } */
