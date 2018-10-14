/* Timer process to return seconds.milliseconds for timing keystrokes
   in emacs.  Gives unique timestamps for 99999 seconds (27 hours or so),
   as seconds.milliseconds. 

   Fri 10-1-93 - return only timestamp (format in emacs); add a seconds digit
   Sat 11-14-92 - Erik Altmann */

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>

main()
{
    char c;
    struct timeval null_timeout, time;
    fd_set readfds;

    null_timeout.tv_sec = 0;    /* man select for info  */
    null_timeout.tv_usec = 0;
   
    while (1) {

        FD_ZERO(&readfds);      /* zero readfs, which is set by select */
        FD_SET(fileno(stdin), &readfds);
        select(1, &readfds, 0, 0, &null_timeout); /* block on stdin */
        getchar();
        gettimeofday(&time, 0);
        /* sssss.mmm, then keystroke stuff padded on the right, then up to
           28 characters of command, padded on the right to put the 
           effects column at 52.  HERE: WARNING: if this changes update
           occurrences of "effects column" in log.el. */
        printf("%05d.%03d",
               time.tv_sec % 100000,
               time.tv_usec / 1000);
    }
}
