       IDENTIFICATION DIVISION.
       PROGRAM-ID. Backalley.
       AUTHOR-ID. Oshaboy.
       DATE-WRITTEN. 2025-12-06.
      *Remarks. A breakout clone I wrote in COBOL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78 WS_PADDLE_MAX_SPEED VALUE 12. 
       78 WS_PADDLE_MAX_NSPEED VALUE -12.
       78 WS_PADDLE_Y VALUE 550.
       78 WS_PADDLE_WIDTH VALUE 90.
       78 WS_PADDLE_HEIGHT VALUE 10.
       78 WS_BALL_SIZE VALUE 10.
       78 WS_SCREEN_WIDTH VALUE 800.
       78 WS_SCREEN_HEIGHT VALUE 600.
       78 WS_LIVES_POSITION_X VALUE 700.
       78 WS_LIVES_POSITION_Y VALUE 580.
       78 WS_SDL_INIT_EVERYTHING VALUE 62001.
       78 WS_WINDOWPOS_CENTERED VALUE 805240832.
       78 SIZEOF_INT VALUE LENGTH OF BINARY-INT.  
       78 WS_BRICK_WIDTH VALUE 70.
       78 WS_BRICK_HEIGHT VALUE 30.
       78 WS_INITIAL_BRICK_COUNT VALUE 40.

       01 WS_SDL_TICKS PIC 9(10).
       01 WS_TIME_OF_LAST_FRAME PIC 9(10) VALUE 0.
       01 WS_TIME_OF_LAST_BALL PIC 9(10) VALUE 0.
       01 WS_TIME_SINCE_LAST_BALL PIC 9(10) VALUE 0.
       01 WS_WAIT_TIME PIC S9(10).
       01 WS_WINDOW_TITLE PIC X(10) VALUE Z'Backalley'.
       01 WS_THANKS PIC X(19) VALUE 'Thanks for playing!'.
       01 WS_WINDOW USAGE POINTER.
       01 WS_RENDERER USAGE POINTER.
           
       01 WS_MOUSE_POSITION_X PIC 9(3).
       01 WS_EXIT_STATUS PIC 9 VALUE 0.
           88 WS_CONTINUE VALUE 0.
           88 WS_EXIT VALUE 1.
       01 WS_BOOL_RESULT USAGE BINARY-CHAR UNSIGNED.
           88 WS_BOOL_RESULT_FALSE VALUE 0.
           88 WS_BOOL_RESULT_TRUE VALUE 1 THRU 255.
       01 WS_MOUSE_STATUS PIC 9.
           88 WS_MOUSE_NO_CHANGE VALUE 0.
           88 WS_MOUSE_DOWN VALUE 1.
           88 WS_MOUSE_UP VALUE 2.

       01 WS_PADDLE_POSITION_X PIC S9(3)V99 VALUE 370.
       01 WS_PADDLE_DELTA PIC S9(3)V99.
       01 WS_LIVES_COUNT PIC 9 VALUE 3.
       01 WS_PADDLE_RECT.
           02 X USAGE BINARY-INT SYNC.
           02 Y USAGE BINARY-INT VALUE WS_PADDLE_Y.
           02 W USAGE BINARY-INT VALUE WS_PADDLE_WIDTH.
           02 H USAGE BINARY-INT VALUE WS_PADDLE_HEIGHT.
       01 WS_IS_BALL_IN_GAME PIC 9 VALUE 0.
           88 WS_BALL_IS_IN_GAME VALUE 1.
           88 WS_BALL_ISNT_IN_GAME VALUE 0.
       01 WS_BALL_RECT.
           02 X USAGE BINARY-INT.
           02 Y USAGE BINARY-INT.
           02 W USAGE BINARY-INT VALUE WS_BALL_SIZE.
           02 H USAGE BINARY-INT VALUE WS_BALL_SIZE.
       01 WS_BALL_POSITION.
           02 X PIC S9(3)V99.
              88 WS_BALL_INITIAL_POSITION_X VALUE 395.
           02 Y PIC S9(3)V99.
              88 WS_BALL_INITIAL_POSITION_Y VALUE 400.
       01 WS_LIVES_RECT.
           02 X USAGE BINARY-INT.
           02 Y USAGE BINARY-INT VALUE 580.
           02 W USAGE BINARY-INT VALUE WS_BALL_SIZE.
           02 H USAGE BINARY-INT VALUE WS_BALL_SIZE.
       01 WS_BALL_DIRECTION.
           02 WS_BALL_X_DIRECTION PIC S9V99.
           02 WS_BALL_Y_DIRECTION PIC S9V99.

       01 WS_EVENT.
           02 WS_EVENT_TYPE USAGE BINARY-INT UNSIGNED.
               88 WS_EVENT_SDL_QUIT VALUE 256.
               88 WS_EVENT_SDL_MOUSE_MOTION VALUE 1024.
               88 WS_EVENT_SDL_MOUSE_BUTTON VALUE 1025 THRU 1026.
               88 WS_EVENT_SDL_MOUSE_BUTTON_DOWN VALUE 1025.
               88 WS_EVENT_SDL_MOUSE_BUTTON_UP VALUE 1026.
           02 WS_EVENT_TIMESTAMP USAGE BINARY-INT UNSIGNED.
           02 FILLER PIC X(48).
       01 E_MOUSE_MOTION_EVENT REDEFINES WS_EVENT.
           02 FILLER PIC X(20).
           02 X USAGE BINARY-INT.
           02 Y USAGE BINARY-INT.
           02 XREL USAGE BINARY-INT.
           02 YREL USAGE BINARY-INT.
           02 FILLER PIC X(20).
       01 E_MOUSE_BUTTON_EVENT REDEFINES WS_EVENT.
           02 FILLER PIC X(16).
           02 BUTTON USAGE BINARY-CHAR UNSIGNED.
               88 MOUSE_BUTTON_LEFT VALUE 1.
           02 STATE USAGE BINARY-CHAR UNSIGNED.
           02 CLICKS USAGE BINARY-CHAR UNSIGNED.
           02 FILLER USAGE BINARY-CHAR.
           02 X USAGE BINARY-INT.
           02 Y USAGE BINARY-INT.
           02 FILLER PIC X(28).
       01 WS_EVENT_STATUS USAGE BINARY-INT.
           88 WS_NO_EVENT_PENDING VALUE 0.
           88 WS_EVENT_PENDING VALUE 1.
       01 WS_BALL_SPEED PIC 99 VALUE 2.
       01 WS_TOTAL_BRICKS PIC 99 VALUE WS_INITIAL_BRICK_COUNT.
       01 WS_BRICKS OCCURS WS_INITIAL_BRICK_COUNT
                    INDEXED BY WS_BRICKS_I.
           02 WS_BRICK.
               03 WS_BRICK_STATUS PIC 9.
                   88 WS_BRICK_EXISTS VALUE 0.
                   88 WS_BRICK_DESTROYED VALUE 1.
               03 WS_BRICK_RECT.
                   04 X USAGE BINARY-INT.
                   04 Y USAGE BINARY-INT.
                   04 W USAGE BINARY-INT.
                   04 H USAGE BINARY-INT.
       01 WS_GAME_STATE PIC 9 VALUE 0.
           88 WS_PLAYING VALUE 0.
           88 WS_WIN VALUE 1.
           88 WS_LOSE VALUE 2.
       01 WIN_SQUARE_RECT.
           02 X USAGE BINARY-INT.
           02 Y USAGE BINARY-INT.
           02 W USAGE BINARY-INT.
           02 H USAGE BINARY-INT.
       01 WIN_SQUARE_COLOR.
           02 R USAGE BINARY-CHAR UNSIGNED.
           02 G USAGE BINARY-CHAR UNSIGNED.
           02 B USAGE BINARY-CHAR UNSIGNED.                

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM INIT
           PERFORM UNTIL WS_EXIT
               PERFORM INPUT_HANDLING
               PERFORM STATE_HANDLING
               PERFORM IDLE
               PERFORM DRAW
               PERFORM STATE_CHECK
           END-PERFORM
           PERFORM CLEANUP
           STOP RUN.
       INIT SECTION.
           CALL STATIC 'SDL_Init' USING
               BY VALUE SIZE SIZEOF_INT WS_SDL_INIT_EVERYTHING
           END-CALL
           CALL STATIC 'SDL_CreateWindowAndRenderer' USING 
               BY VALUE SIZE SIZEOF_INT WS_SCREEN_WIDTH 
               BY VALUE SIZE SIZEOF_INT WS_SCREEN_HEIGHT 
               BY VALUE SIZE SIZEOF_INT 2
               BY REFERENCE WS_WINDOW
               BY REFERENCE WS_RENDERER 
           END-CALL
           CALL STATIC 'SDL_SetWindowTitle' USING
               BY VALUE WS_WINDOW
               BY REFERENCE WS_WINDOW_TITLE
           END-CALL
           CALL STATIC 'SDL_SetWindowPosition' USING
               BY VALUE WS_WINDOW
               BY VALUE SIZE SIZEOF_INT WS_WINDOWPOS_CENTERED
               BY VALUE SIZE SIZEOF_INT WS_WINDOWPOS_CENTERED
           END-CALL
           PERFORM VARYING WS_BRICKS_I FROM 0 
                                       BY   1
                             UNTIL WS_BRICKS_I >= WS_INITIAL_BRICK_COUNT
           
               SET WS_BRICK_EXISTS(WS_BRICKS_I + 1) TO TRUE
               COMPUTE X IN WS_BRICK_RECT(WS_BRICKS_I + 1) = 
                   (FUNCTION MOD(WS_BRICKS_I 8) * 
                       (WS_BRICK_WIDTH + 10)) + 80
               COMPUTE Y IN WS_BRICK_RECT(WS_BRICKS_I + 1) =
                   FUNCTION INTEGER(WS_BRICKS_I / 8)
                       * (WS_BRICK_HEIGHT + 10) + 40
               MOVE WS_BRICK_HEIGHT
                   TO H IN WS_BRICK_RECT(WS_BRICKS_I + 1)
               MOVE WS_BRICK_WIDTH
                   TO W IN WS_BRICK_RECT(WS_BRICKS_I + 1)
           END-PERFORM.

       CLEANUP SECTION.
           CALL STATIC 'SDL_RenderClear' USING
               BY VALUE WS_RENDERER 
           END-CALL
           CALL STATIC 'SDL_DestroyWindow' USING
               BY VALUE WS_WINDOW
           END-CALL 
           CALL STATIC 'SDL_Quit' END-CALL
           DISPLAY WS_THANKS.
       DRAW SECTION.
           IF NOT WS_WIN
           THEN
               CALL STATIC 'SDL_SetRenderDrawColor' USING
                   BY VALUE WS_RENDERER
                   BY VALUE SIZE 1 -56
                   BY VALUE SIZE 1 -56
                   BY VALUE SIZE 1 -1
                   BY VALUE SIZE 1 -1
               END-CALL
               CALL STATIC 'SDL_RenderClear' USING
                   BY VALUE WS_RENDERER
               END-CALL
           ELSE 
               MULTIPLY FUNCTION RANDOM BY WS_SCREEN_WIDTH GIVING 
                   X IN WIN_SQUARE_RECT
               MULTIPLY FUNCTION RANDOM BY WS_SCREEN_HEIGHT GIVING 
                   Y IN WIN_SQUARE_RECT
               MULTIPLY FUNCTION RANDOM BY 19 GIVING 
                   W IN WIN_SQUARE_RECT
                   H IN WIN_SQUARE_RECT
               ADD 1 TO W IN WIN_SQUARE_RECT
               ADD 1 TO H IN WIN_SQUARE_RECT
               MULTIPLY FUNCTION RANDOM BY 256 GIVING
                   R IN WIN_SQUARE_COLOR
               MULTIPLY FUNCTION RANDOM BY 256 GIVING
                   G IN WIN_SQUARE_COLOR
               MULTIPLY FUNCTION RANDOM BY 256 GIVING
                   B IN WIN_SQUARE_COLOR
               CALL STATIC 'SDL_SetRenderDrawColor' USING
                   BY VALUE WS_RENDERER
                   BY VALUE SIZE 1 R IN WIN_SQUARE_COLOR
                   BY VALUE SIZE 1 G IN WIN_SQUARE_COLOR
                   BY VALUE SIZE 1 B IN WIN_SQUARE_COLOR
                   BY VALUE SIZE 1 -1
               END-CALL
               CALL 'SDL_RenderFillRect' USING
                   BY VALUE WS_RENDERER
                   BY REFERENCE WIN_SQUARE_RECT
               END-CALL 
           END-IF
           CALL STATIC 'SDL_SetRenderDrawColor' USING
               BY VALUE WS_RENDERER
               BY VALUE SIZE 1 20
               BY VALUE SIZE 1 20
               BY VALUE SIZE 1 20
               BY VALUE SIZE 1 -1
           END-CALL
           MOVE WS_PADDLE_POSITION_X TO X IN WS_PADDLE_RECT
           CALL STATIC 'SDL_RenderFillRect' USING
               BY VALUE WS_RENDERER
               BY REFERENCE WS_PADDLE_RECT
           END-CALL
           MOVE X IN WS_BALL_POSITION TO X IN WS_BALL_RECT
           MOVE Y IN WS_BALL_POSITION TO Y IN WS_BALL_RECT
           IF WS_BALL_IS_IN_GAME
           THEN
               CALL STATIC 'SDL_RenderFillRect' USING
                   BY VALUE WS_RENDERER
                   BY REFERENCE WS_BALL_RECT
               END-CALL
           END-IF
           MOVE WS_LIVES_POSITION_X TO X IN WS_LIVES_RECT
           CALL STATIC 'SDL_SetRenderDrawColor' USING
               BY VALUE WS_RENDERER
               BY VALUE SIZE 1 -1
               BY VALUE SIZE 1 100
               BY VALUE SIZE 1 100
               BY VALUE SIZE 1 -1
           END-CALL
           PERFORM WS_LIVES_COUNT TIMES 
               CALL STATIC 'SDL_RenderFillRect' USING
                   BY VALUE WS_RENDERER
                   BY REFERENCE WS_LIVES_RECT
               END-CALL
               ADD 15 TO X IN WS_LIVES_RECT
           END-PERFORM
           PERFORM VARYING WS_BRICKS_I FROM 1 
                                       BY 1
                              UNTIL WS_BRICKS_I > WS_INITIAL_BRICK_COUNT
               IF WS_BRICK_EXISTS(WS_BRICKS_I)
               THEN

                   CALL STATIC 'SDL_RenderFillRect' USING
                       BY VALUE WS_RENDERER
                       BY REFERENCE WS_BRICK_RECT(WS_BRICKS_I)
                   END-CALL
               END-IF

           END-PERFORM
           CALL STATIC 'SDL_RenderPresent' USING 
               BY VALUE WS_RENDERER
           END-CALL.
       INPUT_HANDLING SECTION. 
           
           CALL STATIC 'SDL_PollEvent' USING
               BY REFERENCE WS_EVENT
               RETURNING WS_EVENT_STATUS
           END-CALL
           SET WS_MOUSE_NO_CHANGE TO TRUE
           PERFORM UNTIL WS_NO_EVENT_PENDING
               EVALUATE TRUE
                   WHEN WS_EVENT_SDL_QUIT
                       SET WS_EXIT TO TRUE
                   WHEN WS_EVENT_SDL_MOUSE_MOTION
                       MOVE X IN E_MOUSE_BUTTON_EVENT TO
                                 WS_MOUSE_POSITION_X
                   WHEN WS_EVENT_SDL_MOUSE_BUTTON_DOWN
                       AND MOUSE_BUTTON_LEFT
                           SET WS_MOUSE_DOWN TO TRUE
                   WHEN WS_EVENT_SDL_MOUSE_BUTTON_UP
                       AND MOUSE_BUTTON_LEFT
                           SET WS_MOUSE_UP TO TRUE
               END-EVALUATE 
               CALL STATIC 'SDL_PollEvent' USING
                   BY REFERENCE WS_EVENT
                   RETURNING WS_EVENT_STATUS
               END-CALL
           END-PERFORM.


       STATE_HANDLING SECTION.
      * MOVE THE PADDLE * 
           COMPUTE WS_PADDLE_DELTA =
               ((WS_MOUSE_POSITION_X - (W IN WS_PADDLE_RECT / 2) -
                   WS_PADDLE_POSITION_X)) / 8
           IF WS_PADDLE_DELTA > WS_PADDLE_MAX_SPEED 
           THEN
               MOVE WS_PADDLE_MAX_SPEED TO WS_PADDLE_DELTA
           END-IF
           IF WS_PADDLE_DELTA < WS_PADDLE_MAX_NSPEED 
           THEN
               MOVE WS_PADDLE_MAX_NSPEED TO WS_PADDLE_DELTA
           END-IF
           ADD WS_PADDLE_DELTA TO WS_PADDLE_POSITION_X
      * SERVE BALL IF READY *
           IF WS_PLAYING AND WS_BALL_ISNT_IN_GAME AND WS_MOUSE_DOWN 
           THEN 
               SET WS_BALL_IS_IN_GAME TO TRUE
               SET WS_BALL_INITIAL_POSITION_X TO TRUE
               SET WS_BALL_INITIAL_POSITION_Y TO TRUE 
               COMPUTE WS_BALL_X_DIRECTION = (FUNCTION RANDOM - 0.5) * 2
               MOVE 1 TO WS_BALL_Y_DIRECTION
               CALL STATIC 'SDL_GetTicks'
                   RETURNING WS_TIME_OF_LAST_BALL
               END-CALL
               SUBTRACT 1 FROM WS_LIVES_COUNT
           END-IF
      * MOVE THE BALL * 
           IF WS_BALL_IS_IN_GAME 
           THEN 
               COMPUTE X IN WS_BALL_POSITION = WS_BALL_X_DIRECTION * 
                   WS_BALL_SPEED + (X IN WS_BALL_POSITION)
               COMPUTE Y IN WS_BALL_POSITION = WS_BALL_Y_DIRECTION * 
                   WS_BALL_SPEED + (Y IN WS_BALL_POSITION)
           END-IF
      * CHECK BALL COLLISIONS * 
           IF Y IN WS_BALL_POSITION <= 0
           THEN
               MULTIPLY -1 BY WS_BALL_Y_DIRECTION 
           END-IF
           IF X IN WS_BALL_POSITION <= 0
           THEN
               MULTIPLY -1 BY WS_BALL_X_DIRECTION 
           END-IF
           IF X IN WS_BALL_POSITION > (WS_SCREEN_WIDTH - WS_BALL_SIZE)
           THEN
               MULTIPLY -1 BY WS_BALL_X_DIRECTION 
           END-IF
           IF
               X IN WS_BALL_POSITION >= WS_PADDLE_POSITION_X AND
               X IN WS_BALL_POSITION <
                   WS_PADDLE_POSITION_X + WS_PADDLE_WIDTH AND
               Y IN WS_BALL_POSITION >= WS_PADDLE_Y - WS_BALL_SIZE AND 
               Y IN WS_BALL_POSITION < 
                   WS_PADDLE_Y + WS_PADDLE_HEIGHT - WS_BALL_SIZE
           THEN
               MULTIPLY -1 BY WS_BALL_Y_DIRECTION 
               COMPUTE WS_BALL_X_DIRECTION =
                   WS_PADDLE_DELTA / 3
           END-IF
           IF Y IN WS_BALL_POSITION > WS_SCREEN_HEIGHT
           THEN 
               SET WS_BALL_ISNT_IN_GAME TO TRUE
           END-IF
           
           PERFORM VARYING WS_BRICKS_I FROM 1 
                                       BY 1
                              UNTIL WS_BRICKS_I > WS_INITIAL_BRICK_COUNT
               IF WS_BRICK_EXISTS(WS_BRICKS_I)
               THEN 
                   IF
                       X IN WS_BALL_POSITION + WS_BALL_SIZE >=
                           X IN WS_BRICK_RECT(WS_BRICKS_I) AND
                       X IN WS_BALL_POSITION <
                           X IN WS_BRICK_RECT(WS_BRICKS_I) +
                               WS_BRICK_WIDTH AND
                       Y IN WS_BALL_POSITION + WS_BALL_SIZE >=
                           Y IN WS_BRICK_RECT(WS_BRICKS_I) AND
                       Y IN WS_BALL_POSITION <
                           Y IN WS_BRICK_RECT(WS_BRICKS_I) +
                               WS_BRICK_HEIGHT
                           
                           SUBTRACT 1 FROM WS_TOTAL_BRICKS
                           SET WS_BRICK_DESTROYED(WS_BRICKS_I) TO TRUE

                           IF 
                               Y IN WS_BALL_POSITION < 
                                  Y IN WS_BRICK_RECT(WS_BRICKS_I) OR 
                               Y IN WS_BALL_POSITION >=
                                   Y IN WS_BRICK_RECT(WS_BRICKS_I) +
                                       WS_BRICK_HEIGHT -
                                       WS_BALL_SIZE

                                   MULTIPLY -1 BY WS_BALL_Y_DIRECTION 
                           ELSE 
                               IF 
                               X IN WS_BALL_POSITION < 
                                   X IN WS_BRICK_RECT(WS_BRICKS_I) OR 
                               X IN WS_BALL_POSITION >=
                                   X IN WS_BRICK_RECT(WS_BRICKS_I) +
                                       WS_BRICK_WIDTH -
                                       (WS_BALL_SIZE / 2)
                               THEN 
                                   MULTIPLY -1 BY WS_BALL_X_DIRECTION 
                           END-IF   
       
                       END-IF
                   END-IF 
               END-IF
           END-PERFORM
           CALL STATIC 'SDL_GetTicks'
               RETURNING WS_SDL_TICKS
           END-CALL
           SUBTRACT WS_SDL_TICKS FROM WS_TIME_OF_LAST_BALL GIVING
               WS_TIME_SINCE_LAST_BALL
           IF FUNCTION REM(WS_TIME_SINCE_LAST_BALL 10000) = 0
           THEN
               ADD 1 TO WS_BALL_SPEED
           END-IF.


       IDLE SECTION.
           
           CALL STATIC 'SDL_GetTicks'
               RETURNING WS_SDL_TICKS
           END-CALL
           COMPUTE WS_WAIT_TIME =
               17 - (WS_SDL_TICKS - WS_TIME_OF_LAST_FRAME)
           IF WS_WAIT_TIME > 0 
           THEN 
               CALL STATIC 'SDL_Delay' 
                   USING BY VALUE SIZE SIZEOF_INT WS_WAIT_TIME
               END-CALL
           END-IF
           CALL STATIC 'SDL_GetTicks'
               RETURNING WS_TIME_OF_LAST_FRAME
           END-CALL.
       STATE_CHECK SECTION.
           IF WS_PLAYING
           THEN
               IF WS_TOTAL_BRICKS = 0
               THEN
                   SET WS_WIN TO TRUE
               END-IF
               IF WS_LIVES_COUNT = 0 AND WS_BALL_ISNT_IN_GAME 
               THEN
                   SET WS_LOSE TO TRUE
               END-IF
           END-IF.
               
       END PROGRAM Backalley.
