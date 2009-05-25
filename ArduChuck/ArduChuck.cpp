#include "ArduChuck.h"
#include "WProgram.h"

extern "C" {
	#include <inttypes.h>
	#include "WConstants.h"
}

#include "Wire.h"

#define B_MASK 0x1
#define BZ_SHIFT 0
#define BC_SHIFT 1

#define A_MASK 0x03
#define AX_SHIFT 2
#define AY_SHIFT 4
#define AZ_SHIFT 6

#define MIN_STICK_X 0x1B
#define MAX_STICK_X 0xE2
#define MID_STICK_X 0x78

#define MIN_STICK_Y 0x21
#define MAX_STICK_Y 0xDD
#define MID_STICK_Y 0x7E

#define MIN_ACC_X 0x0
#define MID_ACC_X 0xC8
#define MAX_ACC_X 0x400

#define MIN_ACC_Y 0x0
#define MID_ACC_Y 0xC8
#define MAX_ACC_Y 0x400

#define MIN_ACC_Z 0x0
#define MID_ACC_Z 0xC8
#define MAX_ACC_Z 0x400

uint8_t ArduChuck::SX = MID_STICK_X;
uint8_t ArduChuck::SY = MID_STICK_Y;
uint16_t ArduChuck::AX = MID_ACC_X;
uint16_t ArduChuck::AY = MID_ACC_Y;
uint16_t ArduChuck::AZ = MID_ACC_Z;
uint8_t ArduChuck::BZ = 1;
uint8_t ArduChuck::BC = 1;

static uint8_t data[6];
uint8_t ms;
uint16_t ma;

// Constructor
ArduChuck::ArduChuck() {
}

void ArduChuck::begin(uint8_t maxStick, uint16_t maxAcc) {
	
	ms = maxStick;
	ma = maxAcc;

	Wire.beginTransmission(0x52);
	Wire.send(0xF0);
	Wire.send(0x55);
	Wire.endTransmission();

	delay(1);
  
	Wire.beginTransmission(0x52);
	Wire.send(0xFB);
	Wire.send(0x00);
	Wire.endTransmission();
  
	delay(1);  
  
	Wire.beginTransmission(0x52);
	Wire.send(0xFA);
	Wire.endTransmission();
  
	delay(1);
	
	Wire.beginTransmission(0x52);
	Wire.send(0xF0);
	Wire.send(0xAA);
	Wire.endTransmission();
	
	delay(1);

	Wire.beginTransmission(0x52);
	Wire.send(0x40);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.endTransmission();
  
	delay(1);
  
	Wire.beginTransmission(0x52);
	Wire.send(0x46);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.endTransmission();
  
	delay(1);
  
	Wire.beginTransmission(0x52);
	Wire.send(0x4C);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.send(0x00);
	Wire.endTransmission();

	delay(1);
}

uint8_t ArduChuck::decode(uint8_t coded) {
	return (coded ^ 0x17) + 0x17;
}

uint8_t ArduChuck::getButton(uint8_t data, uint8_t shift) {
	return (data >> shift) & 0x1;
}

uint16_t ArduChuck::getAcc(uint8_t ms, uint8_t ls, uint8_t shift) {
	return ms << 2 | ((ls >> shift) & 0x3);
}

void ArduChuck::refresh() {
	Wire.beginTransmission(0x52);
	Wire.send(0x00);
	Wire.endTransmission();
	
	delay(1);

	Wire.requestFrom(0x52, 6);
	int i;
	for(i = 0; (i < 6) && (Wire.available()); i++) {
		data[i] = decode(Wire.receive());
	}

	ArduChuck::SX = map(data[0], MIN_STICK_X, MAX_STICK_X, 0, ms);
	ArduChuck::SY = map(data[1], MIN_STICK_Y, MAX_STICK_Y, 0, ms);
	ArduChuck::AX = map(getAcc(data[2], data[5], AX_SHIFT), MIN_ACC_X, MAX_ACC_X, 0, ma);
	ArduChuck::AY = map(getAcc(data[3], data[5], AY_SHIFT), MIN_ACC_Y, MAX_ACC_Y, 0, ma);
	ArduChuck::AZ = map(getAcc(data[4], data[5], AZ_SHIFT), MIN_ACC_Z, MAX_ACC_Z, 0, ma);
	ArduChuck::BZ = getButton(data[5], BZ_SHIFT);
	ArduChuck::BC = getButton(data[5], BC_SHIFT);
}

ArduChuck Chuck = ArduChuck();