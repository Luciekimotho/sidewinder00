#ifndef ArduChuck_h
#define ArduChuck_h

#include<inttypes.h>

class ArduChuck {
	private:
		uint8_t decode(uint8_t coded);
		uint8_t getButton(uint8_t data, uint8_t shift);
		uint16_t getAcc(uint8_t ms, uint8_t ls, uint8_t shift);
	public:
		static uint8_t SX;
		static uint8_t SY;
		static uint16_t AX;
		static uint16_t AY;
		static uint16_t AZ;
		static uint8_t BZ;
		static uint8_t BC;

		ArduChuck();
		void begin(uint8_t maxStick, uint16_t maxAcc);
		void refresh();
};

extern ArduChuck Chuck;

#endif