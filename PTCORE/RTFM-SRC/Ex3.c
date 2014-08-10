// RTFM-core for RTFM-PT
// RTFM-Resources
enum resources {R,RES_NR};
int ceilings[RES_NR] = {1};
// RTFM-Entry points
enum entry_nr {task1_nr, task2_nr, ENTRY_NR};
int entry_prio[] = {1, 1};
void task1();
void task2();
ENTRY_FUNC entry_func[] = {task1, task2};

// RTFM-Application 
// top level code 
 
#include <stdio.h>  // printf, etc.
#include <unistd.h> // sleep, etc.

#define USER_RESET

void user_reset() {
	printf("User reset\n");
	
RTFM_pend(task2_nr);
// top level code 

	//
RTFM_pend(task2_nr);
// top level code 

	sleep(1);
	
RTFM_pend(task1_nr);
// top level code 


	while (1) {
		sleep(1);
	}
	printf("User reset exit\n");
}

void task1() {
printf("task1\n");
RTFM_lock(R);
printf("task1 R claimed\n");


RTFM_unlock(R);


}
void task2() {
printf("task2\n");
RTFM_lock(R);
printf("task2 R claimed\n");
sleep(5);
printf("task2 R released\n");


RTFM_unlock(R);


}












