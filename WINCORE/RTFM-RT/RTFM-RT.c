/*
 * RTFM-RT.c
 * WIN32 run-time system for RTFM-core
 * (C) 2014 Per Lindgren, Marcus Lindner
 */
#include <stdio.h>
#include <windows.h>
#include <stdio.h>

#ifdef DEBUG
#define DP(fmt, ...) {fprintf(stderr, "\t\t"fmt"\n", ##__VA_ARGS__);}
#define DF(x) x
#else
#define DP(fmt, ...)
#define DF(x)
#endif

#ifdef DEBUG_RT
#define DPT(fmt, ...) {fprintf(stderr, "\t\t\t\t"fmt"\n", ##__VA_ARGS__);}
#else
#define DPT(fmt, ...)
#endif

#include "RTFM-WIN.h"
#include "../autogen.c"

/* data structures for the threads and semaphores */
HANDLE			res_mutex[RES_NR];
HANDLE			pend_sem[ENTRY_NR];

void error(char* s) {
	printf("RTFM_CORE Internal error %s : %d\n", s, GetLastError());
	while (1); // don't exit so we can observe error from within Visual studio
}

/* RTFM_API */
void RTFM_lock(int r) {
	DP("Claim     :%s", res_names[r]);
	if (WaitForSingleObject(
		res_mutex[r],		// handle to semaphore
		INFINITE			// block forever
		)) error("WaitForSingleObject: Mutex");
}

void RTFM_unlock(int r) {
	DP("Release   :%s", res_names[r]);
	if (!ReleaseMutex(
			res_mutex[r]	// handle to semaphore
	)) error("ReleaseMutex");
}

void RTFM_pend(int t) {
	DP("Pend      :%s", entry_names[t]);
	BOOL b = ReleaseSemaphore(
		pend_sem[t],		// handle to semaphore
		1,					// increase count by one
		NULL				// not interested in previous count
		);
	if (!b)
		DPT("ReleaseSemaphore: exceeded max number");
}

DWORD WINAPI MyThreadFunction(LPVOID lpParam) {
	int id = (int)lpParam;
	DPT("Working thread %d started : Task %s", id, entry_names[id]);
	while (1) {
		DPT("Task blocked (awaiting invocation): %s", entry_names[id]);
		if (WaitForSingleObject(
			pend_sem[id],	// handle to semaphore
			INFINITE		// block forever
			)) error("WaitForSingleObject: Sempahore");
		entry_func[id]();	// dispatch the task
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

int main() {
	int i;
	for (i = 0; i < RES_NR; i++) {
		if (!(res_mutex[i] = CreateMutex(
			NULL,					// no security descriptor
			FALSE,					// mutex not owned
			(LPCWSTR) res_names[i]	// object name
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
	
	for (i = 0; i < ENTRY_NR; i++) {
		if (!(pend_sem[i] = CreateSemaphore(
			NULL,					// default security attributes
			0,						// initial count
			1,  					// maximum count
			(LPCWSTR)entry_names[i]	// named semaphore
			))) error("CreateSemaphore error");

		if (!(hThreadArray[i] = CreateThread(
			NULL,											// default security attributes
			0,												// use default stack size  
			(LPTHREAD_START_ROUTINE)MyThreadFunction,       // thread function name
			(LPVOID)i,										// argument to thread function 
			0,												// use default creation flags 
			0												// returns the thread identifier
			))) error("CreateThread(): new thread failed");

		DPT("thread %d created\n", i)
	}
	
#ifdef USER_RESET
	user_reset();
#endif

	while (1)
		;
}
