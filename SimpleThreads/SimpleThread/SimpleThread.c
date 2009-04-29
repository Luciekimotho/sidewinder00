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
#import "SimpleThread.h"

#define MAX_THREADS 10

struct ThreadInfo {
	struct SimpleThread simpleThread;
	void (*threadFunction)(struct SimpleThread*);
};

struct ThreadInfo threads[MAX_THREADS];
uint8_t threadNumber = 0;
uint8_t currentThread = 0;
uint8_t flag = 0;

int addNewThread(void (*threadFunction)(struct SimpleThread*)) {
	if (threadNumber < MAX_THREADS) {
		threads[threadNumber].simpleThread.currentPosition = 0;
		threads[threadNumber].threadFunction = threadFunction;
		threadNumber++;
		return 0;
	} else {
		return -1;
	}
}

void executeThreads() {
	while(1) {
		if (currentThread == threadNumber) {
			currentThread = 0;
		}
		threads[currentThread].threadFunction(&(threads[currentThread].simpleThread));
		currentThread++;
	}
}

SIMPLE_THREAD(testThread) {
	ST_START
	sleep(1);
	printf("Thread1\n");
	flag = 1;
	ST_END
}

SIMPLE_THREAD(testThread2) {
	ST_START
	printf("Thread 2 after initialization\n");
	ST_WAITFOR(flag == 1)
	printf("Condition met!\n");
	ST_YIELD
	printf("Thread 2 after yielding\n");
	flag = 0;
	ST_END
}

int main(int argc, char** argv) {

	printf("Configuring the thread\n");
	addNewThread(testThread);
	addNewThread(testThread2);
	executeThreads();

	return 0;
}
