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

struct ThreadInfo threadSlots[MAX_THREADS];
uint8_t threadNumber = 0;
uint8_t currentThread = 0;

void setupDataStructures() {
	uint8_t i;
	for (i = 0; i < MAX_THREADS; i++) {
		threadSlots[i].threadFunction = 0;
	}
}

void removeThread(uint8_t threadNumber) {
	if (threadNumber < MAX_THREADS) {
		threadSlots[threadNumber].threadFunction = 0;
	}
}

int getThreadSlot() {
	int i;
	for (i = 0; i < MAX_THREADS; i++) {
		if (threadSlots[i].threadFunction == 0) {
			return i;
		}
	}
	return -1;
}

uint8_t addNewThread(void (*threadFunction)(struct SimpleThread*), void *localVarPointer) {
	if (threadNumber < MAX_THREADS) {
		int freeThreadSlot = getThreadSlot();
		if (freeThreadSlot > -1 && freeThreadSlot < MAX_THREADS) {
			threadSlots[threadNumber].simpleThread.currentPosition = 0;
			threadSlots[threadNumber].simpleThread.threadNumber = threadNumber;
			threadSlots[threadNumber].threadFunction = threadFunction;
			threadSlots[threadNumber].simpleThread.localVariables = localVarPointer;
			threadNumber++;
			return 0;
		} else {
			return -1;
		}
	} else {
		return -1;
	}
}

void executeThreads() {
	while(1) {
		if (currentThread == threadNumber) {
			currentThread = 0;
		}
		if (threadSlots[currentThread].threadFunction != 0) {
			threadSlots[currentThread].threadFunction(&(threadSlots[currentThread].simpleThread));
		}
		currentThread++;
	}
}

