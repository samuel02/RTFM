/*
 * RTFM-PT.c
 * Pthread run-time system implementation for RTFM-core
 * (C) 2014 Per Lindgren, Marcus Lindner (WINAPI)
 *
 * DEBUG		enable tracing of RTFM_RT calls (Application <-> RTFM_RT)
 * DEBUG_RT		enable tracing of POSIX calls   (RTFM <-> pthreads library)
 * OSX			when defined the _GNU_SOURCE is not defined (which is OK under OSX)
 *
 * Linux
 *  Ctrl-z to escape to terminal
 *  sudo pkill -9 -f PTCORE to kill dangling process
 *
 * OSX
 *  Ctrl-c to kill
 */

#define min(a, b) (a < b ? a : b)
#define max(a, b) (a > b ? a : b)
typedef enum {
	false, true
} bool;

#ifdef TRACE
#ifdef TRACE_TIME
#define DP(fmt, ...) {fprintf(stderr, "\t\t\t\t<%f> "fmt"\n", RT_time_to_float(time_get()), ##__VA_ARGS__);}
#define DF(x) x
#else
#define DP(fmt, ...) {fprintf(stderr, "\t\t\t\t"fmt"\n", ##__VA_ARGS__);}
#define DF(x) x
#endif
#else
#define DP(fmt, ...)
#define DF(x)
#endif

#ifdef TRACE_OS
#ifdef TRACE_TIME
#define DPT(fmt, ...) {fprintf(stderr, "\t\t\t\t\t\t\t\t\t<%f> "fmt"\n",  RT_time_to_float(time_get()), ##__VA_ARGS__);}
#else
#define DPT(fmt, ...) {fprintf(stderr, "\t\t\t\t\t\t\t\t\t"fmt"\n", ##__VA_ARGS__);}
#endif
#else
#define DPT(fmt, ...)
#endif

#ifndef OSX
#define _GNU_SOURCE
#endif

#include <unistd.h>			// sleep etc.
#include <stdio.h>			// printf etc.
#include <stdlib.h>			// rand etc.
// we are not sure the application includes the RTFM-RT
#include <time.h>			// for random seed
#include <sys/time.h>       // gettimeofday

#include "RTFM-PT.h"

#include "../Application/autogen.c"

#include <pthread.h>
#include <errno.h>
#include <string.h>
#include <semaphore.h>
#include <fcntl.h>			// for O_CREATE etc.

typedef struct timeval timeval_t;

void handle_error_en(int en, char *msg) {
	fprintf(stderr, "en = %d, errno = %d\n", en, errno);
	perror("errno");
	errno = en;
	perror(msg);
	exit(EXIT_FAILURE);
}

/* data structures for the timer */
RTFM_time base_line[ENTRY_NR];
RTFM_time dead_line[ENTRY_NR];
RTFM_time new_base_line[ENTRY_NR];
RTFM_time new_dead_line[ENTRY_NR];
bool aborted[ENTRY_NR];

//RTFM_time after[ENTRY_NR];
//RTFM_time after_base_line[ENTRY_NR];
RTFM_time global_base_line = 0;

/* data structures for the threads and semaphores */
pthread_mutex_t res_mutex[RES_NR];
sem_t* pend_sem[ENTRY_NR];
sem_t* reset_sem[ENTRY_NR];
pthread_mutex_t pend_count_mutex[ENTRY_NR];
int pend_count[ENTRY_NR] = { 0 };

/* mutex management primitves*/
void m_lock(pthread_mutex_t *m) {
	int e;
	if ((e = pthread_mutex_lock(m)))
		handle_error_en(e, "pthread_mutex_lock");
}

void m_unlock(pthread_mutex_t *m) {
	int e;
	if ((e = pthread_mutex_unlock(m)))
		handle_error_en(e, "pthread_mutex_unlock");
}

void s_post(sem_t *s) {
	int e;
	if ((e = sem_post(s)))
		handle_error_en(e, "sem_post");
}

void s_wait(sem_t *s) {
	int e;
	if ((e = sem_wait(s)))
		handle_error_en(e, "sem_wait");
}

/* RTFM_API */
void RTFM_lock(int f, int r) {
	DP("Claim    :%s->%s", entry_names[f], res_names[r]);DPT("RTFM_lock   :pthread_mutex_lock(&res_mutex[%d])", r);
	m_lock(&res_mutex[r]);
	DPT("RTFM_lock   :pthread_mutex_locked(&res_mutex[%d])", r);DP("Claimed  :%s->%s", entry_names[f], res_names[r]);

}

void RTFM_unlock(int f, int r) {
	DP("Release  :%s<-%s", entry_names[f],res_names[r]);
	DPT("RTFM_unlock :pthread_mutex_unlock(res_mutex[%d])", r)
	m_unlock(&res_mutex[r]);
}

RTFM_msg RTFM_async(RTFM_time a, RTFM_time b, int f, int t) {
	int lcount;
	DP("Async     :%s->%s", entry_names[f], entry_names[t]);

	DPT("RTFM_async  :pthread_mutex_lock(&pend_count_mutex[%d])", t);
	m_lock(&pend_count_mutex[t]);
	{   // inside lock of the counter
		lcount = pend_count[t];
		if (lcount == 0) {
			DPT("RTFM_pend   :pend_count[%d]++", t);
			pend_count[t]++; // may not be atomic, hence needs a lock

			// for good measure, we work on the timing information under the lock
			DPT("after_bl[%2d]  = %f", f, RT_time_to_float(base_line[f]));DPT("after[%2d|     = %f", f, RT_time_to_float(a));

			new_base_line[t] = RT_time_add(base_line[f], a); // set the baseline for the receiver
			new_dead_line[t] = new_base_line[t] + b;
		}
	}DPT("RTFM_pend   :pthread_mutex_unlock(&pend_count_mutex[%d])", t);
	m_unlock(&pend_count_mutex[t]);

	if (lcount == 0) { // just a single outstanding semaphore/mimic the single buffer pend of interrupt hardware
		DPT("RTFM_pend   :pend_sem[%d], Post :semaphore posted %s", t, entry_names[t])
		s_post(pend_sem[t]);
	} else {
		DPT("RTFM_pend   :Post :semaphore discarded, already outstanding");
	}
	aborted[t] = false;
	return t;
}

bool RTFM_abort(int id) {
	aborted[id] = true;
	return true; // for the time being
}

void RTFM_pend(RTFM_time b, int f, int t) {
	int lcount;
	DP("Pend     :%s->%s", entry_names[f], entry_names[t]);

	DPT("RTFM_pend   :pthread_mutex_lock(&pend_count_mutex[%d])", t);
	m_lock(&pend_count_mutex[t]);
	{   // inside lock of the counter
		lcount = pend_count[t];
		if (lcount == 0) {
			DPT("RTFM_pend   :pend_count[%d]++", t);
			pend_count[t]++; // may not be atomic, hence needs a lock

			// for good measure, we work on the timing information under the lock

			new_base_line[t] = time_get(); // set the baseline for the receiver
			new_dead_line[t] = new_base_line[t] + b;
			DPT("after_bl[%2d]  = %f", f, RT_time_to_float(base_line[f]));

		}
	}DPT("RTFM_pend   :pthread_mutex_unlock(&pend_count_mutex[%d])", t);
	m_unlock(&pend_count_mutex[t]);

	if (lcount == 0) { // just a single outstanding semaphore/mimic the single buffer pend of interrupt hardware
		DPT("RTFM_pend   :pend_sem[%d], Post :semaphore posted %s", t, entry_names[t])
		s_post(pend_sem[t]);
	} else {
		DPT("RTFM_pend   :Post :semaphore discarded, already outstanding");
	}
}

RTFM_time RTFM_get_bl(int id) {
	return base_line[id];
}

RTFM_time time_get();
void RTFM_set_bl(int id) {
	RTFM_time new_bl = time_get();
	DPT("RTFM_set_bl   :base_line[%d] = %f)", id, RT_time_to_float(new_bl));
	base_line[id] = new_bl;
}

/* run-time system implementation */

/* timer related, portable on Linux and OSX */

RTFM_time time_get() {
	int e;
	timeval_t ts;
	if ((e = gettimeofday(&ts, NULL)))
		handle_error_en(e, "gettimeofday");
	return (ts.tv_sec * RT_sec + ts.tv_usec) - global_base_line;
}

void set_global_baseline() {
	int e;
	timeval_t ts;
	if ((e = gettimeofday(&ts, NULL)))
		handle_error_en(e, "gettimeofday");
	global_base_line = (ts.tv_sec * RT_sec + ts.tv_usec);
}

// 100 this is an arbitrarily chosen constant 0.1 ms, Linus/OSX is sloppy conf. bare metal
// the effect is that we may release the task 0.1 ms too soon, on the other hand we reduce overhead
const int jitter = 100;

void time_usleep(RTFM_time t) {
	int e;
	if (t > jitter) {
		DPT("usleep         (%f)", RT_time_to_float(t));
		if ((e = usleep(t)))
			handle_error_en(e, "usleep");
	} else {
		DPT("release time expired or interval to short %f", RT_time_to_float(t));
	}
}

int over_run(int id) {
	RTFM_time now = time_get();
	if (now > dead_line[id]) {
		fprintf(stderr,
				"Deadline expired %s, task/message base_line = %f, dead_line = %f, cur_time = %f\n",
				entry_names[id], RT_time_to_float(base_line[id]),
				RT_time_to_float(dead_line[id]), RT_time_to_float(now));
		return true;
	}
	return false;
}

void *thread_handler(void *id_ptr) {
	int id = *((int *) id_ptr);
	DPT("thread      :Working thread %d started, in reset mode : Task %s", id, entry_names[id]);
	s_wait(reset_sem[id]);
	DPT("thread      :Working thread %d started, in run mode : Task %s", id, entry_names[id]);
	while (1) {
		DP("Task wait:%s", entry_names[id]);DPT("thread      :sem_wait(pend_sem[%d])", id);
		s_wait(pend_sem[id]);
		// consume the semaphore (decrement it's value)
		DPT("thread      :pthread_mutex_lock(&pend_count_mutex[%d])", id);
		RTFM_time offset;
		m_lock(&pend_count_mutex[id]);
		{   // inside lock of the counter
			DPT("thread      :pend_count[%d]--", id);
			pend_count[id]--; // may not be atomic, hence needs a lock

			// for good measure we work on the timing information under the lock
			DPT("old_bl[%2d]    = %f", id, RT_time_to_float(base_line[id]));

			base_line[id] = new_base_line[id];
			dead_line[id] = new_dead_line[id];

			DPT("new_bl[%2d]    = %f", id, RT_time_to_float(base_line[id]));
			RTFM_time cur_time = time_get();
			DPT("cur_time      = %f", RT_time_to_float(cur_time));

			offset = base_line[id] - cur_time;
		}DPT("thread      :pthread_mutex_unlock(&pend_count_mutex[%d])", id);
		m_unlock(&pend_count_mutex[id]);

		DPT("time_usleep(offset)   = %f", RT_time_to_float(offset));
		time_usleep(offset);
		if (aborted[id]) {
			DP("Task aborted %s", entry_names[id]);
			aborted[id] = true;
		} else {
			DPT("thread      :entry_func[%d](%d)", id, id);DP("Task run :%s", entry_names[id]);
			entry_func[id](id); // dispatch the task

#ifdef ABORT_DL
			if (over_run(id)) {
				fprintf(stderr, "Aborting on failing to meet deadline!\n");
				exit(EXIT_FAILURE);
			}
#elif WARN_DL
			over_run(id);
#endif
		}
	}
	return NULL;
}

void dump_priorities() {
	int i;
	fprintf(stderr, "\nResource ceilings:\n");
	for (i = 0; i < RES_NR; i++)
		fprintf(stderr, "Res %d \t ceiling %d : %s\n", i, ceilings[i],
				res_names[i]);
	fprintf(stderr, "\nTask priorities:\n");
	for (i = 0; i < ENTRY_NR; i++)
		fprintf(stderr, "Task %d \tpriority %d : %s\n", i, entry_prio[i],
				entry_names[i]);
}

int main() {
	fprintf(stderr, "PTCORE, RTFM-RT Per Lindgren (C) 2014 \n");
	fprintf(stderr, "%s", CORE_FILE_INFO);
	fprintf(stderr, "PTCORE run-time options :");
#ifdef TRACE
	fprintf(stderr, " -TRACE");
#endif
#ifdef TRACE_TIME
	fprintf(stderr, " -TRACE_TIME");
#endif
#ifdef TRACE_OS
	fprintf(stderr, " -TRACE_OS");
#endif
#ifdef WARN_DL
	fprintf(stderr, " -WARN_DL");
#endif
#ifdef ABORT_DL
	fprintf(stderr, " -ABORT_DL");
#endif

	fprintf(stderr, "\n");

	// Commodity random function provide for the exmaples
	srand(time(NULL));

	int policy = SCHED_FIFO; // SCHED_RR; //SCHED_OTHER;
	int p_max = sched_get_priority_max(policy);
	int p_min = sched_get_priority_min(policy);
	int s, i;

	DF(
			fprintf(stderr, "POSIX priorities: np_min %d, p_max %d\n\n", p_min, p_max );
			dump_priorities();
	);

	/* re-mapping of logic priorities to POSIX priorities */
	for (i = 0; i < RES_NR; i++)
		ceilings[i] = min(p_max, ceilings[i] + p_min);
	for (i = 0; i < ENTRY_NR; i++)
		entry_prio[i] = min(p_max, entry_prio[i] + p_min);

	DF(
			fprintf(stderr, "\nAfter re-mapping priorities to POSIX priorities.\n");
			dump_priorities();
	);

	/* Resource management */
	pthread_mutexattr_t mutexattr;
	DPT("main        :pthread_mutexattr_init(&mutexattr)");
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
	DPT("main        :pthread_mutexattr_setprotocol(&mutexattr, PTHREAD_PRIO_PROTECT)");
	if ((s = pthread_mutexattr_setprotocol(&mutexattr, PTHREAD_PRIO_PROTECT)))
		handle_error_en(s, "pthread_mutexattr_setprotocol");

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
		DPT("main        :pthread_mutexattr_setprioceiling(&mutexattr, ceilings[%d]=%d)", i, ceilings[i]);
		if ((s = pthread_mutexattr_setprioceiling(&mutexattr, ceilings[i])))
			handle_error_en(s, "pthread_mutexattr_setprioceiling");

		/* The pthread_mutex_init() function initialises the mutex referenced by mutex with attributes
		 * specified by attr. If attr is NULL, the default mutex attributes are used; the effect is the
		 * same as passing the address of a default mutex attributes object.
		 * Upon successful initialisation, the state of the mutex becomes initialised and unlocked.
		 */
		DPT("main        :pthread_mutex_init(res_mutex[%d])", i);
		if ((s = pthread_mutex_init(&res_mutex[i], &mutexattr)))
			handle_error_en(s, "pthread_mutex_init");
	}
	/* Task/thread management */
	pthread_t thread[ENTRY_NR];
	int id[ENTRY_NR]; // unique identifier for each thread

	pthread_attr_t attr;
	DPT("main        :pthread_attr_init(&attr)");
	if ((s = pthread_attr_init(&attr)))
		handle_error_en(s, "pthread_attr_init");

	/* SHED_FIFO per priority
	 * Threads scheduled under this policy are chosen from a thread list that is ordered by the time
	 * its threads have been on the list without being executed; generally, the head of the list is
	 * the thread that has been on the list the longest time, and the tail is the thread that has
	 * been on the list the shortest time.
	 */
	DPT("main        :pthread_attr_setschedpolicy(&attr, SCHED_FIFO)");
	if ((s = pthread_attr_setschedpolicy(&attr, SCHED_FIFO)))
		handle_error_en(s, "pthread_attr_setschedpolicy");

	/*
	 * Specifies that the scheduling policy and associated attributes are to be set to the
	 * corresponding values from this attribute object.
	 */
	DPT("main        :pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED)");
	if ((s = pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED)))
		handle_error_en(s, "pthread_attr_setinheritsched");

	/*
	 * Reset the mutex attr, alternatively compute the real pending ceiling
	 */
	DPT("main        :pthread_mutexattr_init(&mutexattr)");
	if ((s = pthread_mutexattr_init(&mutexattr)))
		handle_error_en(s, "pthread_mutexattr_init");

	struct sched_param param;
	char sem_name[32]; // allocated string for semaphore name

	for (i = 2; i < ENTRY_NR; i++) { // 0 = user_reset, 1 = user_idle
		/* semaphore handling */
		/*
		 * The named semaphore named name is removed.
		 */
		if ((s = sem_unlink(entry_names[i]))) {
			DPT("main        :Warning sem_unlinked failed, the named semaphore did not exist\n");
		}
		/*
		 * The named semaphore named name is initialized and opened as specified by
		 * the argument oflag and a semaphore descriptor is returned to the calling process.
		 */
		DPT("main        :sem_open(entry_names[%d], O_CREAT, O_RDWR, 0",i);

		sprintf(sem_name, "sem_reset_%d", i);
		reset_sem[i] = sem_open(sem_name, O_CREAT, O_RDWR, 0);
		if (reset_sem[i] == SEM_FAILED)
			handle_error_en(errno, "sem_open");

		sprintf(sem_name, "sem_run_%d", i);
		pend_sem[i] = sem_open(sem_name, O_CREAT, O_RDWR, 0);
		if (pend_sem[i] == SEM_FAILED)
			handle_error_en(errno, "sem_open");

		DPT("main        :pthread_mutex_init(pend_count_mutex[%d], &mutexattr))", i);
		if ((s = pthread_mutex_init(&pend_count_mutex[i], &mutexattr)))
			handle_error_en(s, "pthread_mutex_init");

		/* pend_count_mutex initialization*/
		DPT("main        :pthread_mutexattr_setprioceiling(&mutexattr, entry_prio[%d]=%d)", i, entry_prio[i]);
		if ((s = pthread_mutexattr_setprioceiling(&mutexattr, entry_prio[i])))
			handle_error_en(s, "pthread_mutexattr_setprioceiling");

		/* pthread initialization */
		param.sched_priority = entry_prio[i];
		DPT("main        :pthread_attr_setschedparam(&attr, &param)");
		if ((s = pthread_attr_setschedparam(&attr, &param)))
			handle_error_en(s, "pthread_attr_setschedparam");

		id[i] = i;
		DPT("main        :pthread_create(&thread[%d], &attr, thread_handler, &id[%d])", i, i);
		if ((s = pthread_create(&thread[i], &attr, thread_handler, &id[i])))
			handle_error_en(s, "pthread_create\n");
	}

	/* Set the main thread at lowest priority (this is the thread that will be our (possibly non-terminating) background task, performed at Reset */
	DPT("main        :pthread_self()");
	pthread_t this_thread = pthread_self();
	param.sched_priority = p_min;

	DPT("main        :pthread_setschedparam(this_thread, SCHED_FIFO, &param)");
	if ((s = pthread_setschedparam(this_thread, SCHED_FIFO, &param)))
		handle_error_en(s, "pthread_setschedparam\n");

	sleep(1); /* let the setup be done until continuing */

	/* set baselines */
	set_global_baseline();
	for (i = 0; i < ENTRY_NR; i++) {
		base_line[i] = 0; // only for good measure
	}

	printf(
			"-----------------------------------------------------------------------------------------------------------------------\n");
	DPT("main        :user_reset(user_reset_nr);");
	user_reset(user_reset_nr);

	// change mode from reset to run
	for (i = 2; i < ENTRY_NR; i++) {
		s_post(reset_sem[i]);
	}

	printf(
			"-----------------------------------------------------------------------------------------------------------------------\n");
	user_idle(user_idle_nr);

	while (1)
		RT_sleep(RT_sec * 1); // zzzzzz to save CPU
	/* code for cleanup omitted, we trust POSIX (Linux/OSX) to do the cleaning */
}
