def getBlink():
    blinking = 0
    blinktime = 0.5 #intended blink duration
    
    my_mouse = mouse()
    mousebutton, clickpos, clicktime = my_mouse.get_click()
    
    ps = eyetracker.pupil_size()
    
    if mousebutton == 1: #override getblink function with left-mouse click
        return 1
    
    if (ps != 0):
        return blinking
    elif (wait_for_blink_end() - wait_for_blink_start() >= blinktime):
        blinking = 1
        return blinking
    return blinking

