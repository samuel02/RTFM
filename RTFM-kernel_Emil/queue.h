/*
 * queue.h
 *
 *  Created on: 20 oct 2014
 *      Author: Emil Fresk
 */

#ifndef QUEUE_H_
#define QUEUE_H_

/*
 * Timer queue structure definition
 */
typedef struct _TQ {
  rt_time_t bl;
  rt_tid_t id;
  struct _TQ* next;
} TQ_t;


/*
 * Function prototypes
 */
void tq_init();
void T_timer_init();

#endif /* QUEUE_H_ */
