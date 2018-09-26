#ifndef _ASYNC_PROCESS_H_
#define _ASYNC_PROCESS_H_

#define _GNU_SOURCE
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <string.h>
#include <stdbool.h>

struct process* create_process(char *const command[], bool nonblock);
void delete_process(struct process *process);
int process_pid(struct process *process);
void process_send_input(struct process *process, const char *string);
const char* process_receive_output(struct process *process);
int process_alive_p(struct process *process);

#endif
