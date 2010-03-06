
#ifndef DISTANCEBASED_H
#define DISTANCEBASED_H

enum 
{
    AM_DISTANCEBASED = 1,
    RESEND_PERIOD = 100,
    SEND_PERIOD = 3000,
    ACK_PERIOD = 10,
};

typedef nx_struct MyMsg
{
    nx_uint8_t source_id;
    nx_uint8_t sequence_number;
    nx_uint8_t hop_id;
} MyMsg;

typedef nx_struct MyAck
{
    nx_uint8_t mittente;
    nx_uint8_t nodo_raggiunto;
} MyAck;

typedef struct Elemento
{
    nx_uint8_t nodo_raggiunto;
    struct Elemento * next_elem;
} Elemento;

#endif
