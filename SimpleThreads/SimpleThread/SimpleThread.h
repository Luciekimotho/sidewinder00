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

#define SIMPLE_THREAD(THREAD_NAME)	void THREAD_NAME(struct SimpleThread* st)

#define ST_START 				switch(st->currentPosition) { \
									case 0:

#define ST_WAITFOR(CONDITION) 		st->currentPosition = __LINE__; \
								case __LINE__: \
									if (!(CONDITION)) { \
										break; \
									}

#define ST_YIELD					st->currentPosition = __LINE__; \
									break; \
								case __LINE__: \

#define ST_END 						st->currentPosition = 0; \
									break; \
								};

#define ST_WAIT_UNTIL(condition) if (condition);

struct SimpleThread {
	uint8_t currentPosition;
};

int addNewThread(void (*threadFunction)(struct SimpleThread*));
void executeThreads();

#endif /* SIMPLETHREAD_H_ */
