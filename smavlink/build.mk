# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the smavlink library.
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, 14 Jan, 2013
#

SMAVLINK_LIB       := libsmavlink.a

SMAVLINK_INCLUDES  += -I$(TOP)/smavlink/include
SMAVLINK_INCLUDES  += -I$(TOP)/smavlink/include/smavlink/messages
SMAVLINK_INCLUDES  += -I$(TOP)/ivory-runtime

SMAVLINK_CFLAGS    += $(SMAVLINK_INCLUDES)

SMAVLINK_OBJECTS :=    \
  src/channel.o \
  src/crc.o \
  src/send.o \
  src/receive.o \
  src/messages/smavlink_message_attitude.o \
  src/messages/smavlink_message_attitude_quaternion.o \
  src/messages/smavlink_message_auth_key.o \
  src/messages/smavlink_message_battery_status.o \
  src/messages/smavlink_message_change_operator_control_ack.o \
  src/messages/smavlink_message_change_operator_control.o \
  src/messages/smavlink_message_command_ack.o \
  src/messages/smavlink_message_command_long.o \
  src/messages/smavlink_message_data_stream.o \
  src/messages/smavlink_message_debug.o \
  src/messages/smavlink_message_debug_vect.o \
  src/messages/smavlink_message_file_transfer_dir_list.o \
  src/messages/smavlink_message_file_transfer_res.o \
  src/messages/smavlink_message_file_transfer_start.o \
  src/messages/smavlink_message_global_position_int.o \
  src/messages/smavlink_message_global_position_setpoint_int.o \
  src/messages/smavlink_message_global_vision_position_estimate.o \
  src/messages/smavlink_message_gps_global_origin.o \
  src/messages/smavlink_message_gps_raw_int.o \
  src/messages/smavlink_message_gps_status.o \
  src/messages/smavlink_message_heartbeat.o \
  src/messages/smavlink_message_highres_imu.o \
  src/messages/smavlink_message_hil_controls.o \
  src/messages/smavlink_message_hil_rc_inputs_raw.o \
  src/messages/smavlink_message_hil_state.o \
  src/messages/smavlink_message_local_position_ned.o \
  src/messages/smavlink_message_local_position_ned_system_global_offset.o \
  src/messages/smavlink_message_local_position_setpoint.o \
  src/messages/smavlink_message_manual_control.o \
  src/messages/smavlink_message_manual_setpoint.o \
  src/messages/smavlink_message_memory_vect.o \
  src/messages/smavlink_message_mission_ack.o \
  src/messages/smavlink_message_mission_clear_all.o \
  src/messages/smavlink_message_mission_count.o \
  src/messages/smavlink_message_mission_current.o \
  src/messages/smavlink_message_mission_item.o \
  src/messages/smavlink_message_mission_item_reached.o \
  src/messages/smavlink_message_mission_request.o \
  src/messages/smavlink_message_mission_request_list.o \
  src/messages/smavlink_message_mission_request_partial_list.o \
  src/messages/smavlink_message_mission_set_current.o \
  src/messages/smavlink_message_mission_write_partial_list.o \
  src/messages/smavlink_message_named_value_float.o \
  src/messages/smavlink_message_named_value_int.o \
  src/messages/smavlink_message_nav_controller_output.o \
  src/messages/smavlink_message_optical_flow.o \
  src/messages/smavlink_message_param_request_list.o \
  src/messages/smavlink_message_param_request_read.o \
  src/messages/smavlink_message_param_set.o \
  src/messages/smavlink_message_param_value.o \
  src/messages/smavlink_message_ping.o \
  src/messages/smavlink_message_raw_imu.o \
  src/messages/smavlink_message_raw_pressure.o \
  src/messages/smavlink_message_rc_channels_override.o \
  src/messages/smavlink_message_rc_channels_raw.o \
  src/messages/smavlink_message_rc_channels_scaled.o \
  src/messages/smavlink_message_request_data_stream.o \
  src/messages/smavlink_message_roll_pitch_yaw_rates_thrust_setpoint.o \
  src/messages/smavlink_message_roll_pitch_yaw_speed_thrust_setpoint.o \
  src/messages/smavlink_message_roll_pitch_yaw_thrust_setpoint.o \
  src/messages/smavlink_message_safety_allowed_area.o \
  src/messages/smavlink_message_safety_set_allowed_area.o \
  src/messages/smavlink_message_scaled_imu.o \
  src/messages/smavlink_message_scaled_pressure.o \
  src/messages/smavlink_message_servo_output_raw.o \
  src/messages/smavlink_message_set_global_position_setpoint_int.o \
  src/messages/smavlink_message_set_gps_global_origin.o \
  src/messages/smavlink_message_set_local_position_setpoint.o \
  src/messages/smavlink_message_set_mode.o \
  src/messages/smavlink_message_setpoint_6dof.o \
  src/messages/smavlink_message_setpoint_8dof.o \
  src/messages/smavlink_message_set_quad_motors_setpoint.o \
  src/messages/smavlink_message_set_quad_swarm_led_roll_pitch_yaw_thrust.o \
  src/messages/smavlink_message_set_quad_swarm_roll_pitch_yaw_thrust.o \
  src/messages/smavlink_message_set_roll_pitch_yaw_speed_thrust.o \
  src/messages/smavlink_message_set_roll_pitch_yaw_thrust.o \
  src/messages/smavlink_message_state_correction.o \
  src/messages/smavlink_message_statustext.o \
  src/messages/smavlink_message_sys_status.o \
  src/messages/smavlink_message_system_time.o \
  src/messages/smavlink_message_vfr_hud.o \
  src/messages/smavlink_message_vicon_position_estimate.o \
  src/messages/smavlink_message_vision_position_estimate.o \
  src/messages/smavlink_message_vision_speed_estimate.o

$(eval $(call library,SMAVLINK))

# vim: set ft=make noet ts=2:
