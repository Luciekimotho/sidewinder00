/*
 * Copyright (c) 2009, Guido Rota
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the sidewinder00 nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY GUIDO ROTA ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL GUIDO ROTA BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "SimpleThread.h"
#include <unistd.h>

uint8_t flag = 0;

SIMPLE_THREAD(testThread) {
	ST_START
	sleep(1);
	printf("Thread1\n");
	flag = 1;
	ST_END
}

SIMPLE_THREAD(testThread2) {
	static uint8_t counter = 0;
	ST_START
	printf("Thread 2 after initialization\n");
	printf("Got here %d times!\n", counter++);
	ST_WAITFOR(flag == 1)
	printf("Condition met!\n");
	flag = 0;
	ST_YIELD
	printf("Thread 2 after yielding\n");
	ST_END
}

int main(int argc, char** argv) {

	printf("Configuring the thread\n");
	addNewThread(testThread);
	addNewThread(testThread2);
	addNewThread(testThread2);
	executeThreads();

	return 0;
}

