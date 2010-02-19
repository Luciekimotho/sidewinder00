#ifndef DISTANCEBASED_H
#define DISTANCEBASED_H

enum 
{
    AM_DISTANCEBASED = 6,
    TIMER_PERIOD = 100
};

typedef nx_struct MyMsg
{
    nx_uint8_t source_id;
    nx_uint8_t sequence_number;
    nx_uint8_t pos_x;
    nx_uint8_t pos_y;
    nx_uint8_t hop_id;
} MyMsg;

#endif
