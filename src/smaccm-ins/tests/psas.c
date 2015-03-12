#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>

#include "psas-packet.h"
#include "smaccm_ins.h"

#define PI 3.14159265358979323

static void die(const char *msg, ...) __attribute__((noreturn));
static void die(const char *msg, ...)
{
	va_list ap;
	va_start(ap, msg);
	vfprintf(stderr, msg, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	exit(EXIT_FAILURE);
}

#define die_unless(cond, ...) do { if(!(cond)) die(__VA_ARGS__); } while(0)

#define ARRAY_SIZE(arr) (sizeof arr / sizeof *arr)

#define FOURCC(a,b,c,d) ((a) << 24 | (b) << 16 | (c) << 8 | (d))

struct header {
	double timestamp;
	uint32_t ID;
	uint16_t data_length;
};

struct adis {
	struct {
		double x, y, z;
	}
	gyro, /* in radian/second */
	acc, /* in meters/second/second */
	mag; /* in milligauss */
};

struct mpl3 {
	double pressure; /* in Pascals */
};

static bool read_header(struct header *header)
{
	struct {
		uint32_t ID;
		uint32_t time_hi;
		uint16_t time_lo;
		uint16_t data_length;
	} __attribute__((packed)) raw_header;

	size_t count = fread(&raw_header, sizeof raw_header, 1, stdin);
	if(!count)
		return false;

	header->timestamp = ((uint64_t) ntohl(raw_header.time_hi) << 16 | ntohs(raw_header.time_lo)) * 1e-9;

	header->ID = ntohl(raw_header.ID);

	header->data_length = ntohs(raw_header.data_length);
	if(header->ID == FOURCC('M','P','L','3') && header->data_length == 0x600)
		header->data_length = 6;

	return true;
}

static void read_adis_payload(struct header *header, struct adis *adis)
{
	ADIS16405Data raw;
	die_unless(header->data_length == sizeof raw, "ADIS message has wrong length %04x", header->data_length);
	die_unless(fread(&raw, sizeof raw, 1, stdin) == 1, "could not read payload of ADIS message");
	adis->gyro.x = 0.05 * PI / 180 * (int16_t) ntohs(raw.gyro_x);
	adis->gyro.y = 0.05 * PI / 180 * (int16_t) ntohs(raw.gyro_y);
	adis->gyro.z = 0.05 * PI / 180 * (int16_t) ntohs(raw.gyro_z);
	adis->acc.x = 0.00333 * 9.80665 * (int16_t) ntohs(raw.acc_x);
	adis->acc.y = 0.00333 * 9.80665 * (int16_t) ntohs(raw.acc_y);
	adis->acc.z = 0.00333 * 9.80665 * (int16_t) ntohs(raw.acc_z);
	adis->mag.x = 5.0e-8 * 10000000 * (int16_t) ntohs(raw.magn_x);
	adis->mag.y = 5.0e-8 * 10000000 * (int16_t) ntohs(raw.magn_y);
	adis->mag.z = 5.0e-8 * 10000000 * (int16_t) ntohs(raw.magn_z);
}

static void read_mpl3_payload(struct header *header, struct mpl3 *mpl3)
{
	MPL3115A2Data raw;
	die_unless(header->data_length == sizeof raw, "MPL3 message has wrong length %x", header->data_length);
	die_unless(fread(&raw, sizeof raw, 1, stdin) == 1, "could not read payload of MPL3 message");
	mpl3->pressure = 1.5625e-5 * 1000 * (int32_t) ntohl(raw.pressure);
}

static double initialize_filter(void)
{
	struct header header;
	double last_timestamp = 0;
	struct adis adis = {};
	struct mpl3 mpl3 = {};
	bool got_adis = false, got_mpl3 = false;

	while(read_header(&header))
	{
		switch(header.ID)
		{
		case FOURCC('A','D','I','S'):
			read_adis_payload(&header, &adis);
			last_timestamp = header.timestamp;
			got_adis = true;
			break;

		case FOURCC('M','P','L','3'):
			read_mpl3_payload(&header, &mpl3);
			got_mpl3 = true;
			break;

		default:
			fseek(stdin, header.data_length, SEEK_CUR);
			continue;
		}

		if(got_adis && got_mpl3)
		{
			kalman_init(
				adis.acc.x, adis.acc.y, adis.acc.z,
				adis.mag.x, adis.mag.y, adis.mag.z,
				mpl3.pressure);
			return last_timestamp;
		}
	}

	die("could not find initial measurements");
}

static void print_float_array(float *arr, unsigned int count)
{
	unsigned int i;
	for(i = 0; i < count; ++i)
		printf(",% 12.6f", arr[i]);
}

static void print_state(double timestamp)
{
	printf("%f", timestamp);
	print_float_array(kalman_state.orient, ARRAY_SIZE(kalman_state.orient));
	print_float_array(kalman_state.vel, ARRAY_SIZE(kalman_state.vel));
	print_float_array(kalman_state.pos, ARRAY_SIZE(kalman_state.pos));
	print_float_array(kalman_state.gyro_bias, ARRAY_SIZE(kalman_state.gyro_bias));
	print_float_array(kalman_state.wind, ARRAY_SIZE(kalman_state.wind));
	print_float_array(kalman_state.mag_ned, ARRAY_SIZE(kalman_state.mag_ned));
	print_float_array(kalman_state.mag_xyz, ARRAY_SIZE(kalman_state.mag_xyz));
	printf("\n");
}

int main(void)
{
	double last_timestamp = initialize_filter();
	print_state(last_timestamp);

	struct header header;
	while(read_header(&header))
	{
		switch(header.ID)
		{
		case FOURCC('A','D','I','S'):
			{
				struct adis adis;
				read_adis_payload(&header, &adis);
				kalman_predict(header.timestamp - last_timestamp,
					adis.gyro.x, adis.gyro.y, adis.gyro.z,
					adis.acc.x, adis.acc.y, adis.acc.z);
				mag_measure(adis.mag.x, adis.mag.y, adis.mag.z);
			}
			last_timestamp = header.timestamp;
			print_state(last_timestamp);
			break;

		case FOURCC('M','P','L','3'):
			{
				struct mpl3 mpl3;
				read_mpl3_payload(&header, &mpl3);
				pressure_measure(mpl3.pressure);
			}
			break;

		default:
			fseek(stdin, header.data_length, SEEK_CUR);
			continue;
		}
	}

	exit(EXIT_SUCCESS);
}
