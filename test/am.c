#include <stdint.h>
#include <stdio.h>
#include <math.h>
#include <rtl-sdr.h>

#define true 1
#define false 0

void demodulate(FILE *f, const uint8_t *buf, int n_read)
{
	for (int j = 0; j+1 < n_read; j+=2)
	{
		double i = (((double) buf[j]) - 127) / 127;
		double q = (((double) buf[j+1]) - 127) / 127;
		double amp = sqrt(i*i + q*q);

		// poor man's downsampling from ~250 kHz to ~50 kHz
		if (j%5 == 0)
			fputc((uint8_t) (amp * 255), f);
	}
}

int main(void)
{
	rtlsdr_dev_t *dev;
	rtlsdr_open(&dev, 0);
	rtlsdr_set_tuner_gain_mode(dev, false);
	rtlsdr_set_agc_mode(dev, true);
	rtlsdr_set_center_freq(dev, 98900000);
	rtlsdr_set_tuner_bandwidth(dev, 0);  // auto
	rtlsdr_set_tuner_gain(dev, 256*128);
	rtlsdr_set_sample_rate(dev, 250000);
	rtlsdr_reset_buffer(dev);

	FILE *f = fopen("data.u8", "wb");

	static const int buf_len = 32*1024;
	uint8_t buf[buf_len];
	for (int i = 0; i < 4; ++i)
	{
		int n_read;
		rtlsdr_read_sync(dev, &buf, buf_len, &n_read);
		printf("%d bytes read\n", n_read);

		demodulate(f, buf, n_read);
	}
	rtlsdr_close(dev);
	return 0;
}
