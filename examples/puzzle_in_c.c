/* A simple 4x4 puzzle game.  Click on a tile to move it into the
   adjacent empty space.  Type control-c to exit.
*/

#include <stdio.h>

/* start_ezd process connects the read side to the "toezd" pipe to stdin and
   the write side of the "fromezd" pipe to stdout, and then exec's ezd.
*/

int  toezd[ 2 ],	/* Used to send commands to ezd */
     fromezd[ 2 ];	/* Used by ezd to report events */

FILE  *out,		/* ezd commands sent on this stream */
      *in;		/* ezd events are read in this stream */

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

/* Puzzle data structures */

int	tile_size = 40;		/* Pixel size of each tile. */

struct { int x; int y } position[ 16 ];
				/* Records the position of each tile. */

int	initial_tiles[] = {10,15,12,3,13,8,7,1,2,14,6,4,9,5,11};
				/* Initial positions of the tiles. */

/* Convert a tile index to a pixel value */

int  tile_to_pixel( x )
	int  x;			/* Tile index */
{
	return ( (x+1)*5+x*tile_size );
}

/* Draw a tile and record it's position in position. */

void  draw_tile( tile, x, y )
	int  tile,		/* Number to write in the tile */
	     x, y;		/* Position */
{
	position[ tile ].x = x;
	position[ tile ].y = y;
	fprintf( out, "(object t%d (fill-rectangle %d %d %d %d blue)",
		 tile, tile_to_pixel( x ), tile_to_pixel( y ), tile_size,
		 tile_size );
	fprintf( out, "(text %d %d %d %d center center \"%d\" white \"8x13bold\"))",
		 tile_to_pixel( x ), tile_to_pixel( y ), tile_size,
		 tile_size, tile );
}

/* Handle events from ezd.  Tiles are moved into the empty space by clicking
   on them with mouse button 1.  A control-C entered from the keyboard
   terminates the program.
*/

void  handle_events() 
{
	char  event[1000],	/* Event string */
	      ignore[100],	/* Ignore converted values */
	      inputchar[ 100 ];	/* Character from keyboard */
	int  n,			/* # items converted */
	     tile,		/* Tile number */
	     save_x, save_y;	/* Used to move a tile */

loop:	fflush( out );
	fgets( event, 1000, in );
	n = sscanf( event, "(CLICK PUZZLE PUZZLE T%d %d %d %d %d %d %d %d %d)",
		    &tile, ignore, ignore, ignore, ignore, ignore, ignore,
		    ignore, ignore );
	if  (n == 9)  {
	   /* Button 1 down on a tile. */
	   if  (abs( position[ 0 ].x-position[ tile ].x )+
	        abs( position[ 0 ].y-position[ tile ].y ) == 1)  {
	      /* Tile was next to the blank space so it can be moved. */
	      save_x = position[ 0 ].x;
	      save_y = position[ 0 ].y;
	      position[ 0 ].x = position[ tile ].x;
	      position[ 0 ].y = position[ tile ].y;
	      draw_tile( tile, save_x, save_y );
	   }
	   goto loop;
	}
	n = sscanf( event, "(KEYPRESS PUZZLE PUZZLE %s %d %d %d %d \"%s\")",
		    ignore, ignore, ignore, ignore, ignore, inputchar );
	if  (n == 6 && inputchar[ 0 ] == 3)  exit();
	goto  loop;
}
	
/* The main program. */

main()
{
	int  puzzle_size = tile_to_pixel( 4 );
	int  x, y,
	     ti = 0;

	/* Start ezd and create the window and drawing. */
	start_ezdprocess();
	fprintf( out, "(window puzzle %d %d fixed-size)", puzzle_size,
		 puzzle_size );
	fputs( "(set-drawing puzzle) (overlay puzzle puzzle)", out );
	fprintf( out, "(object backing (fill-rectangle 0 0 %d %d white))",
		 puzzle_size, puzzle_size );
	fputs( "(when * keypress (log-event))", out );

	/* Draw all tiles in the puzzle. */
	position[ 0 ].x = 0;
	position[ 0 ].y = 0;
	for  (x = 0; x < 4; x++) {
	   for  (y = (x==0)?1:0; y < 4; y++)  {
	      draw_tile( initial_tiles[ ti ], x, y );
	      fprintf( out, "(click T%d 1 (log-event))", initial_tiles[ ti ] );
	      ti++;
	   }
	}

	/* Handle ezd events. */
	handle_events();
}
