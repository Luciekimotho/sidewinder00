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
#ifndef SIMPLETHREAD_H_
#define SIMPLETHREAD_H_

#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#define PASTE(A, B) PASTE_EXPANDED(A, B)

#define PASTE_EXPANDED(A, B) A ## B

#define ADD_NEW_THREAD(THREAD_NAME)		static struct THREAD_NAME ## _local PASTE(local, __LINE__); \
										addNewThread(THREAD_NAME, & PASTE(local, __LINE__)); \

#define NONE uint8_t hiddenVar;

#define ST_GETTHREADNUMBER st->threadNumber

#define SIMPLE_THREAD(THREAD_NAME, LOCAL_VARIABLES)		struct THREAD_NAME ## _local { \
															LOCAL_VARIABLES \
														}; \
														void THREAD_NAME(struct SimpleThread* st)

#define ST_GETLOCAL(THREAD_NAME)			struct THREAD_NAME ## _local *local = (struct THREAD_NAME ## _local *)st->localVariables;


#define ST_INIT					switch(st->currentPosition) { \
									case 0:

#define ST_START						st->currentPosition = 1; \
										break; \
									case 1:

#define ST_WAITFOR(CONDITION) 			st->currentPosition = __LINE__; \
									case __LINE__: \
										if (!(CONDITION)) { \
											break; \
										}

#define ST_YIELD						st->currentPosition = __LINE__; \
										break; \
									case __LINE__: \

#define ST_LOOPOVER 					st->currentPosition = 1; \
										break; \

#define ST_END							removeThread(st->threadNumber); \
										break; \

#define ST_TERMINATOR				};

struct SimpleThread {
	uint8_t currentPosition;
	uint8_t threadNumber;
	void *localVariables;
};

uint8_t addNewThread(void (*threadFunction)(struct SimpleThread*), void *localVarPointer);
void executeThreads();
void removeThread(uint8_t threadNumber);
void setupDataStructures();

#endif /* SIMPLETHREAD_H_ */
