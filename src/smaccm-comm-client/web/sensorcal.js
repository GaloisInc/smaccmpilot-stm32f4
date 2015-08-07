$(function() {

  var GyroRaw = Backbone.Model.extend({
    urlRoot: '/controllable_vehicle_i/gyro_raw_output',
  });
  var GyroCal = Backbone.Model.extend({
    urlRoot: '/controllable_vehicle_i/gyro_output_calibration',
  });
  var GyroOut = Backbone.Model.extend({
    urlRoot: '/controllable_vehicle_i/gyro_output',
  });

  var MagRaw = Backbone.Model.extend({
    urlRoot: '/controllable_vehicle_i/mag_raw_output',
  });
  var MagCal = Backbone.Model.extend({
    urlRoot: '/controllable_vehicle_i/mag_output_calibration',
  });
  var MagOut = Backbone.Model.extend({
    urlRoot: '/controllable_vehicle_i/mag_output',
  });

  window.gyroRaw =
    new GyroRaw({});
  window.gyroCal =
    new GyroCal({});
  window.gyroOut =
    new GyroOut({});

  window.gyroScheduler =
    new Scheduler({ period: false}, [gyroRaw, gyroCal, gyroOut]);
  window.gyroSchedulerButton =
    new SchedulerButtonView({ model: gyroScheduler, el: '#gyro-sch-btn' });

  window.magRaw =
    new MagRaw({});
  window.magCal =
    new MagCal({});
  window.magOut =
    new MagOut({});

  window.magScheduler =
    new Scheduler({ period: false}, [magRaw, magCal, magOut]);
  window.magSchedulerButton =
    new SchedulerButtonView({ model: magScheduler, el: '#mag-sch-btn' });

});
