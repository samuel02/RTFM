//*****************************************************************************
// CCOMP_core_cm3.h
// Copyright Per Lindgren 2014
// Extracted from CMSIS core_cm3.h
//*****************************************************************************

/** \brief  Enable External Interrupt

    This function enables a device specific interupt in the NVIC interrupt controller.
    The interrupt number cannot be a negative value. 

    \param [in]      IRQn  Number of the external interrupt to enable
 */
inline void NVIC_EnableIRQ(IRQn_Type IRQn)
{
  NVIC->ISER[((uint32_t)(IRQn) >> 5)] = (1 << ((uint32_t)(IRQn) & 0x1F)); /* enable interrupt */
}



/** \brief  Disable External Interrupt

    This function disables a device specific interupt in the NVIC interrupt controller.
    The interrupt number cannot be a negative value. 

    \param [in]      IRQn  Number of the external interrupt to disable
 */
inline void NVIC_DisableIRQ(IRQn_Type IRQn)
{
  NVIC->ICER[((uint32_t)(IRQn) >> 5)] = (1 << ((uint32_t)(IRQn) & 0x1F)); 
}


/** \brief  Get Pending Interrupt

    This function reads the pending register in the NVIC and returns the pending bit
    for the specified interrupt. 

    \param [in]      IRQn  Number of the interrupt for get pending
    \return             0  Interrupt status is not pending
    \return             1  Interrupt status is pending
 */
inline uint32_t NVIC_GetPendingIRQ(IRQn_Type IRQn)
{
  return((uint32_t) ((NVIC->ISPR[(uint32_t)(IRQn) >> 5] & (1 << ((uint32_t)(IRQn) & 0x1F)))?1:0)); /* Return 1 if pending else 0 */
}


/** \brief  Set Pending Interrupt

    This function sets the pending bit for the specified interrupt. 
    The interrupt number cannot be a negative value.

    \param [in]      IRQn  Number of the interrupt for set pending
 */
inline void NVIC_SetPendingIRQ(IRQn_Type IRQn)
{
  NVIC->ISPR[((uint32_t)(IRQn) >> 5)] = (1 << ((uint32_t)(IRQn) & 0x1F)); /* set interrupt pending */
}


/** \brief  Clear Pending Interrupt

    This function clears the pending bit for the specified interrupt. 
    The interrupt number cannot be a negative value.

    \param [in]      IRQn  Number of the interrupt for clear pending
 */
inline void NVIC_ClearPendingIRQ(IRQn_Type IRQn)
{
  NVIC->ICPR[((uint32_t)(IRQn) >> 5)] = (1 << ((uint32_t)(IRQn) & 0x1F)); /* Clear pending interrupt */
}


/** \brief  Get Active Interrupt

    This function reads the active register in NVIC and returns the active bit. 
    \param [in]      IRQn  Number of the interrupt for get active
    \return             0  Interrupt status is not active
    \return             1  Interrupt status is active
 */
inline uint32_t NVIC_GetActive(IRQn_Type IRQn)
{
  return((uint32_t)((NVIC->IABR[(uint32_t)(IRQn) >> 5] & (1 << ((uint32_t)(IRQn) & 0x1F)))?1:0)); /* Return 1 if active else 0 */
}


/** \brief  Set Interrupt Priority

    This function sets the priority for the specified interrupt. The interrupt 
    number can be positive to specify an external (device specific) 
    interrupt, or negative to specify an internal (core) interrupt.

    Note: The priority cannot be set for every core interrupt.

    \param [in]      IRQn  Number of the interrupt for set priority
    \param [in]  priority  Priority to set
 */
inline void NVIC_SetPriority(IRQn_Type IRQn, uint32_t priority)
{
  if(IRQn < 0) {
    SCB->SHP[((uint32_t)(IRQn) & 0xF)-4] = ((priority << (8 - __NVIC_PRIO_BITS)) & 0xff); } /* set Priority for Cortex-M  System Interrupts */
  else {
    NVIC->IP[(uint32_t)(IRQn)] = ((priority << (8 - __NVIC_PRIO_BITS)) & 0xff);    }        /* set Priority for device specific Interrupts  */
}


/** \brief  Get Interrupt Priority

    This function reads the priority for the specified interrupt. The interrupt 
    number can be positive to specify an external (device specific) 
    interrupt, or negative to specify an internal (core) interrupt.

    The returned priority value is automatically aligned to the implemented
    priority bits of the microcontroller.

    \param [in]   IRQn  Number of the interrupt for get priority
    \return             Interrupt Priority
 */
inline uint32_t NVIC_GetPriority(IRQn_Type IRQn)
{

  if(IRQn < 0) {
    return((uint32_t)(SCB->SHP[((uint32_t)(IRQn) & 0xF)-4] >> (8 - __NVIC_PRIO_BITS)));  } /* get priority for Cortex-M  system interrupts */
  else {
    return((uint32_t)(NVIC->IP[(uint32_t)(IRQn)]           >> (8 - __NVIC_PRIO_BITS)));  } /* get priority for device specific interrupts  */
}

