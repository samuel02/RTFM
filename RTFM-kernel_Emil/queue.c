/*
 * queue.c
 *
 *  Created on: 20 oct 2014
 *      Author: Emil Fresk
 */


#include "queue.h"

/*
 * Timer queue instantiation
 */
TQ_t  tq[TQ_LEN];   // queue
TQ_t* tq_h = NULL;  // head pointer
TQ_t* tq_f = tq;    // free pointer
TQ_t* tq_n;         // new
TQ_t* tq_c;         // current



/*****************************************************************************
 *
 * Inline functions
 *
 *****************************************************************************/


/*
 * Something something???
 */
volatile int t_brval = 0;

__FORCE_INLINE void T_BREAK(int v) {

  t_brval = v;
}


/*
 * Enable/disable/pend commands for the SysTick
 */
__FORCE_INLINE void T_ENABLE() {

  SYSTICK_CSR |= 1;
}

__FORCE_INLINE void T_DISABLE() {

  SYSTICK_CSR &= ~1;
}

__FORCE_INLINE void T_PEND() {

  ICSR |= (1 << PENDSTSET);
}


/*
 * Set a new countdown time for the SysTick
 */
__FORCE_INLINE void T_SET(rt_time_t t) {

  rt_time_t diff = t - T_CURR();

  if (diff > SYSTICK_MASK) {
    SYSTICK_RELOAD = SYSTICK_MASK; // Set reload to maximum time
  } else {
    if (diff <= 0) {
      T_PEND(); // Trigger directly, alternatively
    }
    SYSTICK_RELOAD = (diff & SYSTICK_MASK)-1;
  }
  SYSTICK_CURRENT = 0; // Write to force reload
}


/*
 * Timer Queue panic halt
 */
__FORCE_INLINE void TQ_panic() {
  while (1)
    T_BREAK(-1); // -1, is panic value
}


/*
 * Add an element to the Timer Queue
 */
__FORCE_INLINE void tq_enq(rt_time_t t, rt_tid_t id) {

  rtfm_lock_t lq;

  RT_lock(lq, 2);

  if (tq_f == NULL) TQ_panic();

  tq_n = tq_f;

  // allocate and fill new node
  tq_n->bl = t;
  tq_n->id = id;
  tq_f = (tq_f)->next;

  if (tq_h == NULL || tq_h->bl - t >= 0) {

    // put first in list
    tq_n->next = tq_h;
    tq_h = tq_n;

    RT_unlock(lq);

    T_ENABLE();
    T_PEND();

    return;
  }

  // insert in middle or last
  tq_c = tq_h;

  while (tq_c->next != NULL && tq_c->next->bl < t) {
    tq_c = tq_c->next;
  }

  tq_n->next = tq_c->next;
  tq_c->next = tq_n;

  RT_unlock(lq);
}


/*
 * Async call
 */
__FORCE_INLINE void RTFM_async(rt_time_t af,
                               rt_time_t be,
                               rt_time_t bl_from,
                               rt_time_t to) {

  tq_enq(af + bl_from, to);
}


/*
 * Remove an element to the Timer Queue
 */
__FORCE_INLINE TQ_t* tq_deq() {

  tq_c = tq_h;

  //if (tq_h != NULL) {
    tq_h = tq_h->next;
    tq_c->next = tq_f;
    tq_f = tq_c;
  //}

  return tq_h;
}


/*
 * Get the top element of the Timer Queue
 */
__FORCE_INLINE TQ_t* tq_top() {

  return tq_h;
}

/*****************************************************************************
 *
 * Standard functions
 *
 *****************************************************************************/

/*
 * Timer Queue initialization
 */
void tq_init() {
  int i;
  for (i = 0; i < TQ_LEN - 1; i++) {
    tq[i].next = &tq[i + 1];
  }
  tq[i].next = NULL;
}


/*
 * DWT Timer, SysTick and Queue initialization
 */
void T_timer_init() {

  //Initialize timer queue
  tq_init();

  // DWT setup: enable DEBUG TIMER
  DWT_CTRL |= 1;
  DWT_CYCCNT = 0;

  // SysTick setup: system clock, generate interrupt
  SYSTICK_CSR = (1 << 2) | (1 << 1);
}
