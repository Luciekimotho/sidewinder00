
#ifndef DISTANCEBASED_H
#define DISTANCEBASED_H

enum 
{
    AM_DISTANCEBASED = 1,
    RESEND_PERIOD = 100,
    SEND_PERIOD = 500,
    PERF_PERIOD = 5000,
};

typedef nx_struct MyMsg
{
    nx_uint8_t source_id;
    nx_uint8_t sequence_number;
    nx_uint8_t hop_id;
} MyMsg;

typedef struct Inibito
{
    nx_uint8_t sequence_number;
    struct Inibito * next;
} Inibito;

typedef struct DaInoltrare
{
    nx_struct MyMsg* msg;
    nx_uint32_t when;
    struct DaInoltrare * next;
} DaInoltrare;

typedef struct SeqRicevuta
{
    nx_uint8_t sequence_number;
    bool S2;
    struct SeqRicevuta * next;
} SeqRicevuta;

typedef struct Inviato
{
    nx_uint8_t sequence_number;
    struct Inviato * next;
} Inviato;

typedef struct Inoltrato
{
    nx_struct MyMsg* msg;
    struct Inoltrato * next;
} Inoltrato;

typedef struct Ricevuto
{
    nx_struct MyMsg* msg;
    struct Ricevuto * next;
} Ricevuto;

#endif
