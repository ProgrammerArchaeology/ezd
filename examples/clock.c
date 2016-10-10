/* A Clock face.  Drag either of the hands with mouse button 1 to set the
   value.  The clock face is drawn with its center at 0,0 in a cartesian
   (Y goes up) coordinate system.  Click button 3 on the background to quit.
   To run:

	csh >cc -o clock clock.c -lm
	csh >clock
*/

#include <stdio.h>
#include <math.h>
#include <string.h>

/* start_ezd process connects the read side to the "toezd" pipe to stdin and
   the write side of the "fromezd" pipe to stdout, and then exec's ezd.
*/

int  toezd[ 2 ],	/* Used to send commands to ezd */
     fromezd[ 2 ];	/* Used by ezd to report events */

FILE  *out,		/* ezd commands are written on this stream */
      *in;		/* ezd events are read from this stream */

void  start_ezdprocess()
{
	pipe( toezd );
	pipe( fromezd );
	if  (fork() == 0)  {
	   /* Child process */
	   close( 0 );			/* Move stdin */
	   dup( toezd[ 0 ] );
	   close( toezd[ 0 ] );
	   close( toezd[ 1 ] );
	   close( 1 );			/* Move stdout */
	   dup( fromezd[ 1 ] );
	   close( fromezd[ 0 ] );
	   close( fromezd[ 1 ] );
	   execlp( "ezd", "ezd", 0 );	/* Start ezd & transfer control */
	   exit( 1 );			/* Error exit */
	}
	/* Parent process */
	close( toezd[ 0 ] );
	out = fdopen( toezd[ 1 ], "w" );
	close( fromezd[ 1 ] );
	in = fdopen( fromezd[ 0 ], "r" );
}

/* Move an angle value specified in radians into the 0-2PI range. */

double zero_2pi( x )
	double  x;
{
	if  (x < 0)  return x+M_PI*2;
	return  x;
}

/* Convert time in minutes to an angle in radians in the range 0-2pi. */

double minute_angle( minute )
	int  minute;
{
	return zero_2pi( M_PI_2 - (minute/30.0)*M_PI );
}

/* Convert an angle in radians into a time in minutes */

int  angle_minute( a )
	double  a;
{
	return (15 - (int)((a*30)/M_PI)) % 60;
}

/* Convert an x,y position in the drawing to an angle in radians. */

double  xy_angle( x, y )
	int  x, y;
{
	return  zero_2pi( atan2( (double)y, (double)x ) );
}

/* Draws a hand on the clock face. */

void  draw_hand( name, length, minute )
	char  *name;
	int  length, minute;
{
	double  angle = minute_angle( minute );

	fprintf( out,
	         "(object %s (fill-polygon 0 0 %f %f %f %f %f %f))",
		 		name,
			        25*cos( angle+.25 ), 25*sin(angle+.25 ),
			        length*cos( angle ), length*sin( angle ),
			        25*cos( angle-.25 ), 25*sin(angle-.25 ) );
}

/* Draws both hands on the clock face. */

void  draw_hands( time )
	int  time;
{
	draw_hand( "minute", 85, time % 60 );
	draw_hand( "hour", 55, (time / 12) % 60 );
}

/* Collect events here. */

void  handle_events( )
{
	int  quit = 0,		/* Set when button 3 is pressed. */
	     time = 23,		/* Initial time. */
	     mark_minute,	/* 0: moving hour hand,
				   1: moving minute hand */
	     delta_t,		/* Change in time in minutes. */
	     x,			/* X coordinate in drawing. */
	     y,			/* Y coordinate in drawing. */
	     ignore;		/* Dummy variable for sscanf. */
	char  event[ 1000 ];	/* Event buffer */
	double  mark_angle,	/* Angle of last mouse position. */
		new_angle,	/* Angle of current mouse position. */
		delta_angle;	/* Change in angle. */

	draw_hands( time );
	while  (quit == 0)  {
	   fflush( out );
	   if (fgets( event, 1000, in ) == NULL)  abort();
	   if  (sscanf( event,
	   		"(BUTTON1DOWN CLOCK-WINDOW CLOCK MINUTE %d %d %d %d)",
			&x, &y, &ignore, &ignore ) == 4)  {
	      /* Start moving the minute hand. */
	      mark_minute = 1;
	      mark_angle = minute_angle( time % 60 );
	   }  else  if  (sscanf( event,
	   		  "(BUTTON1DOWN CLOCK-WINDOW CLOCK HOUR %d %d %d %d)",
				 &x, &y, &ignore, &ignore ) == 4)  {
	      /* Start moving the hour hand */
	      mark_minute = 0;
	      mark_angle = minute_angle( (time/12)%60 );
	   }  else  if  (sscanf( event,
	   		      "(MOTION CLOCK-WINDOW CLOCK COVER %d %d %d %d)",
				 &x, &y, &ignore, &ignore ) == 4)  {
	      /* Move the hands */
	      new_angle = xy_angle( x, y );
	      delta_angle = mark_angle-new_angle;
	      if  (delta_angle < -M_PI)  delta_angle = delta_angle+M_PI*2;
	      if  (delta_angle > M_PI)  delta_angle = delta_angle-M_PI*2;
	      delta_t = delta_angle*(30/M_PI)*((mark_minute) ? 1 : 12);
	      if  (delta_t != 0)  {
	         time = (time+delta_t) % 720;
		 mark_angle = minute_angle( ((mark_minute) ? time : time/12) %
		 			    60 );
		 draw_hands( time );
	      }
	   }  else  if  (sscanf( event,
	   		  "(BUTTON3DOWN CLOCK-WINDOW CLOCK BACK %d %d %d %d)",
				 &ignore, &ignore, &ignore, &ignore ) == 4)  {
	      /* Quit */
	      quit = 1;
	   }  else  {
	      /* Unexpected event, so log it on stderr. */
	      fprintf( stderr, "%s", event );
	   }
	}
}    
	






/* Initial command stream. */

char* ezd_commands[] = {
	 "(window clock-window 200 200 fixed-size)",
	 "(set-drawing clock)",
	 "(overlay clock-window clock)",
	 "(origin clock-window clock 100 100)",
	 "(scale clock-window clock 1 -1 1)",
	 "(object back (fill-arc -100 -100 200 200 0 360 gray95)",
	 "	  (arc -100 -100 200 200 0 360 gray85))",
	 "(object minute)",
	 "(text -60 -60 120 120 left up \"time\" grey60 \"times_italic24\")",
	 "(text -60 -60 120 120 right center \"drifts\" grey60",
	 "	\"times_italic24\")",
	 "(text -60 -60 120 120 left down \"by\" grey60 \"times_italic24\")",
	 "(object hour)",
	 "(object cover)",
	 "(fill-arc -5 -5 10 10 0 360 black)",
	 "(when back button2down",
	 "   (ezd '(postscript clock-window \"clock.psf\")))",
	 "(when back button3down (log-event))",
	 "(when minute button1down  (begin (log-event)",
	 "   (ezd '(object cover",
	 "                 (fill-rectangle -100 -100 200 200 clear)))))",
	 "(when hour button1down  (begin (log-event)",
	 "   (ezd '(object cover",
	 "                 (fill-rectangle -100 -100 200 200 clear)))))",
	 "(when cover enter",
	 "      (if (not *mouse-button1*) (ezd '(object cover))))",
	 "(when cover button1up (ezd '(object cover)))",
	 "(when cover motion (log-event))",
	 NULL };

main()
{
	int  i;		/* Loop index. */

	/* Start ezd. */
	start_ezdprocess();
	/* Issue initial ezd commands. */
	for  (i = 0; ezd_commands[ i ] != NULL; i++)
	   fprintf( out, ezd_commands[ i ] );
	/*  Handle ezd events. */
	handle_events();
	exit( 0 );
}
