/*
* RTFM-RT.h
* WIN32 run-time for RTFM-core
* (C) 2014 Per Lindgren, Marcus Lindner
*/

#include <stdio.h>
#include <windows.h>
#include <stdint.h>

#define DEBUG
#define DEBUG_RT
#define WARN_DL

#ifdef DEBUG
#define DP(fmt, ...) {fprintf(stderr, "\t\t"fmt"\n", ##__VA_ARGS__); fflush(stderr);}
#define DF(x) x
#else
#define DP(fmt, ...)
#define DF(x)
#endif

#ifdef DEBUG_RT
#define DPT(fmt, ...) {fprintf(stderr, "\t\t\t\t"fmt"\n", ##__VA_ARGS__); fflush(stderr);}
#else
#define DPT(fmt, ...)
#endif

#include "RTFM-WT.h"
#include "../Application/autogen.c"


/* data structures for the timer */
RTFM_time base_line[ENTRY_NR];
RTFM_time dead_line[ENTRY_NR];
RTFM_time new_base_line[ENTRY_NR];
RTFM_time new_dead_line[ENTRY_NR];

RTFM_time global_base_line = 0;

/* data structures for the threads and semaphores */
HANDLE			res_mutex[RES_NR];
HANDLE			pend_sem[ENTRY_NR];

void error(char* s) {
	printf("RTFM_CORE Internal error %s : %d\n", s, (int)GetLastError());
	while (1); // don't exit so we can observe error from within Visual studio
}

/* RTFM_API */
void RTFM_lock(int f, int r) {
	DP("Claim   :%s->%s", entry_names[f], res_names[r]);
	if (WaitForSingleObject(
		res_mutex[r],		// handle to semaphore
		INFINITE			// block forever
		)) error("WaitForSingleObject: Mutex");
	DP("Claimed :%s->%s", entry_names[f], res_names[r]);
}

void RTFM_unlock(int f, int r) {
	DP("Release :%s<-%s", entry_names[f], res_names[r]);
	if (!ReleaseMutex(
			res_mutex[r]	// handle to semaphore
	)) error("ReleaseMutex");
}

void RTFM_pend(RTFM_time a, RTFM_time b, int f, int t) {
	new_base_line[t] = base_line[f] + (a / 1000); // set the baseline for the receiver
	new_dead_line[t] = new_base_line[t] + (b / 1000);

	DP("Pend    :%s->%s", entry_names[f], entry_names[t]);
	BOOL check = ReleaseSemaphore(
		pend_sem[t],		// handle to semaphore
		1,					// increase count by one
		NULL				// not interested in previous count
		);

	if (!check)
		DPT("ReleaseSemaphore: exceeded max number");
}

RTFM_time RTFM_get_bl(int id) {
	return base_line[id] * 1000;
}

RTFM_time time_get() {
    return GetTickCount() - global_base_line;
}

void  RTFM_set_bl(int id) {
	RTFM_time new_bl = time_get();
	DPT("RTFM_set_bl   :base_line[%d] = %f)", id, RT_time_to_float(new_bl));
	base_line[id] = new_bl;
}

void set_global_baseline() {
	global_base_line = GetTickCount();
}

int over_run(int id) {
	RTFM_time now = time_get();
	if (now > dead_line[id]) {
		fprintf(stderr,
				"Deadline expired %s, task/message base_line = %f, dead_line = %f, cur_time = %f\n",
				entry_names[id], RT_time_to_float(base_line[id]),
				RT_time_to_float(dead_line[id]), RT_time_to_float(now));
		return TRUE;
	}
	return FALSE;
}

DWORD WINAPI MyThreadFunction(LPVOID lpParam) {
	int id = (int)lpParam;
	DPT("Working thread %d started : Task %s", id, entry_names[id]);
	while (1) {
		DPT("Task blocked (awaiting invocation): %s", entry_names[id]);
		if (WaitForSingleObject(
			pend_sem[id],	// handle to semaphore
			INFINITE		// block forever
			)) error("WaitForSingleObject: Semaphore");


		base_line[id] = new_base_line[id];
		dead_line[id] = new_dead_line[id];

		RTFM_time cur_time = time_get();
		RTFM_time offset = base_line[id] - cur_time;

		if (cur_time < base_line[id])
			Sleep(offset);

		DP("Invoke  :%s", entry_names[id]);
		entry_func[id](id);	// dispatch the task

#ifdef ABORT_DL
		if (over_run(id)) {
			fprintf(stderr, "Aborting on failing to meet deadline!\n");
			exit(EXIT_FAILURE);
		}

#elif defined(WARN_DL)
		over_run(id);
#endif
	}
	return 0L;
}

void dump_priorities() {
	int i;
	printf("\nResource ceilings:\n");
	for (i = 0; i < RES_NR; i++)
		printf("Res %d \t ceiling %d\n", i, ceilings[i]);
	printf("\nTask priorities:\n");
	for (i = 0; i < ENTRY_NR; i++)
		printf("Task %d \tpriority %d \n", i, entry_prio[i]);
}

const int winThreadPriorities[] = {
		THREAD_PRIORITY_IDLE,
		THREAD_PRIORITY_LOWEST,
		THREAD_PRIORITY_BELOW_NORMAL,
		THREAD_PRIORITY_NORMAL,
		THREAD_PRIORITY_ABOVE_NORMAL,
		THREAD_PRIORITY_HIGHEST,
		THREAD_PRIORITY_TIME_CRITICAL
		};

int main() {
	int i;
	for (i = 0; i < RES_NR; i++) {
		if (!(res_mutex[i] = CreateMutex(
			NULL,					// no security descriptor
			FALSE,					// mutex not owned
			(LPCSTR) res_names[i]	// object name
			))) {
			error("CreateMutex : create error");
		}	else {
			if (GetLastError() == ERROR_ALREADY_EXISTS)
				error("CreateMutex(): opened existing mutex");
			else
				DPT("CreateMutex(): new %s mutex successfully created", res_names[i]);
		}
	}

	HANDLE hThreadArray[ENTRY_NR];
	for (i = 2; i < ENTRY_NR; i++) {
		if (!(pend_sem[i] = CreateSemaphore(
			NULL,					// default security attributes
			0,						// initial count
			1,  					// maximum count
			(LPCSTR)entry_names[i]	// named semaphore
			))) error("CreateSemaphore error");

		if (!(hThreadArray[i] = CreateThread(
			NULL,											// default security attributes
			0,												// use default stack size  
			(LPTHREAD_START_ROUTINE)MyThreadFunction,       // thread function name
			(LPVOID)i,										// argument to thread function 
			0,												// use default creation flags 
			0												// returns the thread identifier
			))) error("CreateThread(): new thread failed");

		if (!(SetThreadPriority(
			hThreadArray[i],
			winThreadPriorities[min(sizeof(winThreadPriorities), entry_prio[i])]
			))) error("SetThreadPriority(): set thread priority failed");

		DPT("thread %d created, at prio %d", i, entry_prio[i])
	}
	
	if (!(SetThreadPriority(
		GetCurrentThread(),
		winThreadPriorities[entry_prio[user_reset_nr]]
		))) error("SetThreadPriority(): set thread priority failed");
	DPT("main thread, at prio %d", entry_prio[user_reset_nr])

	DPT("setup complete\n\n")

	/* set baselines */
	set_global_baseline();
	for (i = 0; i < ENTRY_NR; i++) {
		base_line[i] = 0; // only for good measure
	}


	printf("-----------------------------------------------------------------------------------------------------------------------\n");

	user_reset(user_reset_nr);

	user_idle(user_idle_nr);
	while (1)
		;
}
