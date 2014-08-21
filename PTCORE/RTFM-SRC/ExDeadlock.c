// RTFM-core for RTFM-PT
// RTFM-Resources
enum resources {R2,R1,RES_NR};
int ceilings[RES_NR] = {1, 1};
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
        
RTFM_pend(task1_nr);
// top level code 

        
RTFM_pend(task2_nr);
// top level code 

}

void func1(){RTFM_lock(R1);
printf("task1 R1 claimed\n");
RTFM_lock(R2);
printf("task1 R2 claimed\n");


RTFM_unlock(R2);



RTFM_unlock(R1);


}
void task1() {
printf("task1\n");
func1();

}
void task2() {
printf("task2\n");
RTFM_lock(R2);
printf("task2 R2 claimed\n");
sleep(1);
RTFM_lock(R1);
printf("task2 R1 claimed\n");


RTFM_unlock(R1);



RTFM_unlock(R2);


}











