/**
 * @file
 *
 * @brief Example program for io_ev binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * For an example of how I/O bindings are used please see src/libs/notification/example.
 *
 * This example uses two I/O operations:
 * - The "input" operation is a file descriptor watcher that waits for
 *   STDIN_FILENO (stdin) to become readable.
 *   Since input is buffered, this typically happends when the user enters some
 *   text and presses return.
 *   In practice code using the I/O binding will attach non-blocking file
 *   descriptors (e.g. from sockets).
 * - The "output" operation is a timer that prints the last read data every
 *   second.
 *
 */
#include <errno.h>  // error handling
#include <stdio.h>  // printf
#include <string.h> // memset & memcpy
#include <unistd.h> // file descriptor numbers (STDIN_FILENO)

#include <kdbassert.h> // assertions (ELEKTRA_NOT_NULL)
#include <kdbhelper.h> // malloc & free
#include <kdbio.h>     // I/O binding functions (elektraIo*)
#include <kdbio/ev.h>  // I/O binding constructor for ev (elektraIoEvNew)

#include <ev.h> // ev functions

#define BUFFER_LENGTH 255
#define ONE_SECOND 1000

ElektraIoInterface * binding;
ElektraIoFdOperation * input;
ElektraIoTimerOperation * output;

int min (int a, int b)
{
	return (a > b) ? b : a;
}

void stopLoop (void)
{
	// Cleanup
	elektraIoBindingRemoveFd (input);
	elektraIoBindingRemoveTimer (output);
	elektraFree (input);
	elektraFree (output);
	elektraIoBindingCleanup (binding);

	ev_break (EV_DEFAULT, EVBREAK_ONE);
}

void readText (ElektraIoFdOperation * fdOp, int flags ELEKTRA_UNUSED)
{
	printf ("input: file descriptor became readable\n");

	char * lastInput = elektraIoFdGetData (fdOp);
	ELEKTRA_NOT_NULL (lastInput);

	char buffer[BUFFER_LENGTH];
	int bytesRead = read (elektraIoFdGetFd (fdOp), &buffer, BUFFER_LENGTH);
	if (bytesRead != -1)
	{
		// make sure there is a null terminator in buffer
		buffer[min (BUFFER_LENGTH - 1, bytesRead + 1)] = 0;
		// remove newline from string
		buffer[strcspn (buffer, "\r\n")] = 0;
		// copy to lastInput
		memcpy (lastInput, buffer, BUFFER_LENGTH);
	}
	else
	{
		int error = errno;
		if (error != EINTR)
		{
			printf ("input: I/O error occured - exiting\n");
			stopLoop ();
		}
	}
}

void printText (ElektraIoTimerOperation * timerOp)
{
	char * lastInput = elektraIoTimerGetData (timerOp);
	ELEKTRA_NOT_NULL (lastInput);

	if (strcmp (lastInput, "exit") == 0)
	{
		printf ("timer: stopping\n");
		stopLoop ();
	}
	else
	{
		if (strlen (lastInput) > 0)
		{
			printf ("timer: last text was \"%s\"\n", lastInput);
		}
		else
		{
			printf ("timer: text is empty\n");
		}
	}
}

int main (void)
{
	// Initialize buffer
	char lastInput[BUFFER_LENGTH];
	memset (lastInput, 0, BUFFER_LENGTH);

	printf ("Please enter some text and press return.\n");
	printf ("Enter \"exit\" to stop and exit.\n");

	// Create libev event loop
	struct ev_loop * loop = EV_DEFAULT;

	// Initialize I/O binding tied to event loop
	binding = elektraIoEvNew (loop);
	// Read lines from STDIN
	input = elektraIoNewFdOperation (STDIN_FILENO, ELEKTRA_IO_READABLE, 1, readText, &lastInput);
	// Print last read data every second
	output = elektraIoNewTimerOperation (ONE_SECOND, 1, printText, &lastInput);

	// Add operations to binding
	elektraIoBindingAddFd (binding, input);
	elektraIoBindingAddTimer (binding, output);

	// Start the event loop
	ev_run (loop, 0);

	ev_loop_destroy (loop);

	return 0;
}
