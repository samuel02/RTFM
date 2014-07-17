/*
 * RTFM-PT.c
 * Pthread run-time system implementation for RTFM-core
 * (C) 2014 Per Lindgren
 *
 */
#include "RTFM-PT.h"
#define _GNU_SOURCE

//#define COND
#define SEM
#define OSX

#define handle_error_en(en, msg) do { errno = en; perror(msg); exit(EXIT_FAILURE); } while (0)
#define handle_error(msg) do { perror(msg); exit(EXIT_FAILURE); } while (0)
#define min(a, b) (a < b ? a : b)
#define max(a, b) (a > b ? a : b)

//#define DEBUG(x) x
#define DEBUG(x)

#include "../Application/autogen.c"

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <errno.h>
#include <string.h>

pthread_mutex_t res_mutex[RES_NR];
void RTFM_lock(int r) {
	int s;
	if ((s = pthread_mutex_lock(&res_mutex[r])))
		handle_error_en(s, "pthread_mutex_lock\n");
}
void RTFM_unlock(int r) {
	int s;
	if ((s = pthread_mutex_unlock(&res_mutex[r])))
		handle_error_en(s, "pthread_mutex_unlock\n");
}

void m_lock(pthread_mutex_t *m) {
	int s;
	if ((s = pthread_mutex_lock(m)))
		handle_error_en(s, "pthread_mutex_lock\n");
}

void m_unlock(pthread_mutex_t *m) {
	int s;
	if ((s = pthread_mutex_unlock(m)))
		handle_error_en(s, "pthread_mutex_unlock\n");
}

#ifdef COND
pthread_mutex_t pend_mutex[ENTRY_NR];
pthread_cond_t pend_cond[ENTRY_NR] = {PTHREAD_COND_INITIALIZER};

void RTFM_pend(int t) {
	printf("Pend id %d\n", t);
// rendezvous behavior
	pthread_mutex_lock(&pend_mutex[t]);// ensure that the receiver is has finished previous invokation
	pthread_mutex_unlock(&pend_mutex[t]);// releases the lock so that the
	pthread_cond_signal(&pend_cond[t]);// wakes up the receiver, and continues
}

#define handle_error_en(en, msg) do { errno = en; perror(msg); exit(EXIT_FAILURE); } while (0)
int debug = TRUE; // should be an argument to main

// conditional variable synchronization
void *thread_handler(void *id_ptr) {
	int id = *((int *) id_ptr);
	int s;
	printf("thread %d started\n", id);
	while (1) {
		fprintf(stderr, "Thread %d blocked\n", id);
		if ((s = pthread_cond_wait(&pend_cond[id], &pend_mutex[id])))
		handle_error_en(s, "pthread_cond_wait\n");
		entry_func[id](); // dispatch the task
	}
	return NULL;
}
#endif

#ifdef SEM
#include <semaphore.h>
#ifdef OSX
//char *semaphore_name[ENTRY_NR];
#endif
sem_t* pend_sem[ENTRY_NR];
pthread_mutex_t pend_mutex[ENTRY_NR];
int pend_count[ENTRY_NR] = { 0 };

void s_post(sem_t *s) {
	int e;
	if ((e = sem_post(s)))
		handle_error_en(e, "sem_post\n");
}

void s_wait(sem_t *s) {
	int e;
	if ((e = sem_wait(s)))
		handle_error_en(e, "sem_wait\n");
}

void RTFM_pend(int t) {
	int lcount;
	DEBUG( printf("Pend id %d\n", t); )

	m_lock(&pend_mutex[t]);
	lcount = pend_count[t]; // inside lock of the counter
	if (lcount == 0)
		pend_count[t]++;
	m_unlock(&pend_mutex[t]);

	if (lcount == 0) { // just a single outstanding semaphore/mimic the single buffer pend of interrupt hardware
		s_post(pend_sem[t]);
		DEBUG( printf("---> semaphore posted\n"); )
	} else {
		DEBUG( printf("---> semaphore discarded, already outstanding\n"); )
	}
}

void *thread_handler(void *id_ptr) {
	int id = *((int *) id_ptr);
	DEBUG( printf("thread %d started\n", id); )

	while (1) {
		DEBUG( fprintf(stderr, "Thread %d blocked\n", id); )
		s_wait(pend_sem[id]);

		entry_func[id](); // dispatch the task

		m_lock(&pend_mutex[id]);
		pend_count[id]--; // inside lock of the counter
		m_unlock(&pend_mutex[id]);
	}
	return NULL;
}
#endif

void dump_priorities() {
	int i;
	printf("\nResource ceilings:\n");
	for (i = 0; i < RES_NR; i++)
		printf("Res %d \t ceiling %d\n", i,  ceilings[i]);
	printf("\nTask priorities:\n");
	for (i = 0; i < ENTRY_NR; i++)
		printf("Task %d \tpriority %d \n",i, entry_prio[i]);

}
int main() {
	int policy 	= SCHED_FIFO; // SCHED_RR; //SCHED_OTHER;
	int p_max 	= sched_get_priority_max(policy);
	int p_min 	= sched_get_priority_min(policy);
	int s, i;

	DEBUG(
		printf("POSIX priorities: np_min %d, p_max %d\n\n", p_min, p_max);
		printf("Task/ceilings of the source .core program\n");
		dump_priorities();
	)

	/* re-mapping of logic priorities to POSIX priorities */
	for (i = 0; i < RES_NR; i++)
		ceilings[i] = min(p_max, ceilings[i] + p_min);
	for (i = 0; i < ENTRY_NR; i++)
		entry_prio[i] = min(p_max, entry_prio[i] + p_min);

	DEBUG(
		printf("\nAfter re-mapping priorities to POSIX priorities.\n");
		dump_priorities();
	)

	/* Resource management */
	pthread_mutexattr_t mutexattr;
	if ((s = pthread_mutexattr_init(&mutexattr)))
		handle_error_en(s, "pthread_mutexattr_init");
	/*
	 * When a thread owns one or more mutexes initialized with the PTHREAD_PRIO_PROTECT protocol,
	 * it shall execute at the higher of its priority or the highest of the priority ceilings of
	 * all the mutexes owned by this thread and initialized with this attribute,
	 * regardless of whether other threads are blocked on any of these mutexes or not.
	 *
	 * RTFM: Essentially this implements the system ceiling of SRP.
	 */
	if ((s = pthread_mutexattr_setprotocol(&mutexattr, PTHREAD_PRIO_PROTECT)))
		handle_error_en(s, "pthread_mutexattr_setprotocol\n");

	for (i = 0; i < RES_NR; i++) {
		/*
		 * The prioceiling attribute contains the priority ceiling of initialised mutexes.
		 * The values of prioceiling will be within the maximum range of priorities defined by SCHED_FIFO.
		 * The prioceiling attribute defines the priority ceiling of initialised mutexes, which is the
		 * minimum priority level at which the critical section guarded by the mutex is executed.
		 * In order to avoid priority inversion, the priority ceiling of the mutex will be set to a
		 * priority higher than or equal to the highest priority of all the threads that may lock that
		 * mutex.
		 *
		 * The values of prioceiling will be within the maximum range of priorities defined under the SCHED_FIFO
		 * scheduling policy.
		 */

		if ((s = pthread_mutexattr_setprioceiling(&mutexattr, ceilings[i])))
			handle_error_en(s, "pthread_mutexattr_setprioceiling\n");

		/* The pthread_mutex_init() function initialises the mutex referenced by mutex with attributes
		 * specified by attr. If attr is NULL, the default mutex attributes are used; the effect is the
		 * same as passing the address of a default mutex attributes object.
		 * Upon successful initialisation, the state of the mutex becomes initialised and unlocked.
		 */

		if ((s = pthread_mutex_init(&res_mutex[i], &mutexattr)))
			handle_error_en(s, "pthread_mutex_init\n");
	}
	/* Task/thread management */
	pthread_t thread[ENTRY_NR];

	int id[ENTRY_NR]; // unique identifier for each thread

	pthread_attr_t attr;
	if ((s = pthread_attr_init(&attr)))
		handle_error_en(s, "attr_init");

	/* SHED_FIFO per priority
	 * Threads scheduled under this policy are chosen from a thread list that is ordered by the time
	 * its threads have been on the list without being executed; generally, the head of the list is
	 * the thread that has been on the list the longest time, and the tail is the thread that has
	 * been on the list the shortest time.
	 */
	if ((s = pthread_attr_setschedpolicy(&attr, SCHED_FIFO)))
		handle_error_en(s, "pthread_attr_setschedpolicy");

	struct sched_param param;
	/*
	 * Specifies that the scheduling policy and associated attributes are to be set to the
	 * corresponding values from this attribute object.
	 */
	if ((s = pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED)))
		handle_error_en(s, "pthread_attr_setinheritsched");

	for (i = 0; i < ENTRY_NR; i++) {
		/* semaphore handling */
#ifdef SEM
#ifdef OSX
		char str[32];

		if ((sprintf(str, "RTFM_SEM_%d",i)) < 0)
			handle_error_en(errno, "sprintf_failed\n");

		/*
		 * The named semaphore named name is removed.
		 */
		DEBUG( printf("Semaphore str len %d :%s\n", (int) strlen(str), str); )
		if ((s = sem_unlink(str)))
			handle_error_en(errno, "sem_unlink");

		/*
		 * The named semaphore named name is initialized and opened as specified by
		 * the argument oflag and a semaphore descriptor is returned to the calling process.
		 */
		pend_sem[i] = sem_open(str, O_CREAT, O_RDWR, 0);
		if (pend_sem[i] == SEM_FAILED)
			handle_error_en(errno, "sem_open");

#else
		/* The sem_init() function is used to initialise the unnamed semaphore referred to by sem.
		 * The value of the initialised semaphore is value. Following a successful call to sem_init(),
		 * the semaphore may be used in subsequent calls to sem_wait(), sem_trywait(), sem_post(),
		 * and sem_destroy(). This semaphore remains usable until the semaphore is destroyed. If the
		 * pshared argument has a non-zero value, then the semaphore is shared between processes;
		 * in this case, any process that can access the semaphore sem can use sem for performing
		 * sem_wait(), sem_trywait(), sem_post(), and sem_destroy() operations.
		 */
		if ((s = sem_init(&pend_sem[i], 0, 0)))
			handle_error_en(s, "sem_init");
#endif
#endif
		/* pend_mutex initialization*/
		if ((s = pthread_mutex_init(&pend_mutex[i], &mutexattr)))
			handle_error_en(s, "pthread_mutex_init\n");

		/* pthread initialization */
		param.sched_priority = entry_prio[i];

		if ((s = pthread_attr_setschedparam(&attr, &param)))
			handle_error_en(s, "pthread_attr_setschedparam");
		id[i] = i;
		if ((s = pthread_create(&thread[i], &attr, thread_handler, &id[i])))
			handle_error_en(s, "pthread_create\n");

		DEBUG( printf("thread %d created\n", i); )
	}

#ifdef USER_RESET
	user_reset();
#endif

	/* code for cleanup omitted, we trust linux/OSX to do the cleaning */

	/*
	 for (i = 0; i < ENTRY_NR; i++) {
	 if (s = pthread_join(thread[i], NULL))
	 handle_error_en(s, "pthread_join");
	 }
	 */
	return 0;
}
