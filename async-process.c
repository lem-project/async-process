#include "async-process.h"

struct process {
  int fd;
  char *pty_name;
  char buffer[256];
  pid_t pid;
};

static int open_pty(void)
{
  int fd = posix_openpt(O_RDWR | O_CLOEXEC | O_NOCTTY);
  if (fd < 0) return -1;
  if (grantpt(fd) == -1 || unlockpt(fd) == -1) return -1;
  fcntl(fd, F_SETFD, FD_CLOEXEC);
  return fd;
}

static struct process* allocate_process(int fd, const char *pts_name, int pid)
{
  struct process *process = malloc(sizeof(struct process));
  if (process == NULL)
    return NULL;
  process->fd = fd;
  process->pty_name = malloc(strlen(pts_name) + 1);
  process->pid = pid;
  strcpy(process->pty_name, pts_name);
  return process;
}

struct process* create_process(char *const command[])
{
  int pty_master = open_pty();
  if (pty_master == -1)
    return NULL;

  char *pts_name = ptsname(pty_master);
  if (pts_name == NULL)
    return NULL;

  fcntl(pty_master, F_SETFL, O_NONBLOCK);

  int pipefd[2];

  if (pipe(pipefd) == -1) return NULL;

  pid_t pid = fork();

  if (pid == 0) {
    close(pipefd[0]);
    pid = fork();
    if (pid == 0) {
      close(pipefd[1]);
      setsid();
      int pty_slave = open(pts_name, O_RDWR | O_NOCTTY);
      close(pty_master);
      dup2(pty_slave, STDIN_FILENO);
      dup2(pty_slave, STDOUT_FILENO);
      dup2(pty_slave, STDERR_FILENO);
      close(pty_slave);
      execvp(command[0], command);
    } else {
      char buf[12];
      sprintf(buf, "%d", pid);
      write(pipefd[1], buf, sizeof(buf));
      close(pipefd[1]);
      exit(0);
    }
  } else {
    close(pipefd[1]);
    if (waitpid(pid, NULL, 0) == -1)
      return NULL;
    char buf[12];
    read(pipefd[0], buf, sizeof(buf));
    close(pipefd[0]);
    return allocate_process(pty_master, pts_name, atoi(buf));
  }

  return NULL;
}

int process_pid(struct process *process)
{
  return process->pid;
}

void process_send_input(struct process *process, const char *string)
{
  write(process->fd, string, strlen(string));
}

const char* process_receive_output(struct process *process)
{
  int n = read(process->fd, process->buffer, sizeof(process->buffer)-1);
  if (n == -1)
    return NULL;
  process->buffer[n] = '\0';
  return process->buffer;
}

int process_alive_p(struct process *process)
{
  return kill(process->pid, 0) == 0;
} 
