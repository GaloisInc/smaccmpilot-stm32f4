class PidConfig extends React.Component {
  render() {
    return (
      <div className="col-sm-6 col-lg-3">
        <h1>{this.props.name}</h1>
        <form ref="form" onSubmit={ e => e.preventDefault() }>
        <div className="row">
          <div className="col-xs-4"><label>P gain: <input type="number" step="any" onChange={ this.changed.bind(this) } name="p_gain" value={ String(this.props.p_gain) } style={{width: "100%"}} /></label></div>
          <div className="col-xs-4"><label>I gain: <input type="number" step="any" onChange={ this.changed.bind(this) } name="i_gain" value={ String(this.props.i_gain) } style={{width: "100%"}} /></label></div>
          <div className="col-xs-4"><label>D gain: <input type="number" step="any" onChange={ this.changed.bind(this) } name="d_gain" value={ String(this.props.d_gain) } style={{width: "100%"}} /></label></div>
          <div className="col-xs-4"><label>DD gain: <input type="number" step="any" onChange={ this.changed.bind(this) } name="dd_gain" value={ String(this.props.dd_gain) } style={{width: "100%"}} /></label></div>
        </div>
        <div className="row">
          <div className="col-xs-6"><label>I min: <input type="number" step="any" onChange={ this.changed.bind(this) } name="i_min" value={ String(this.props.i_min) } style={{width: "100%"}} /></label></div>
          <div className="col-xs-6"><label>I max: <input type="number" step="any" onChange={ this.changed.bind(this) } name="i_max" value={ String(this.props.i_max) } style={{width: "100%"}} /></label></div>
        </div>
        </form>
      </div>
    )
  }

  changed() {
    var obj = {};
    $('input', this.refs.form.getDOMNode()).each(
      (ix, el) => obj[el.name] = el.value
    )
    this.props.changed(obj)
  }
}

function parseFloats(data) {
  let obj = {}
  for(let key of Object.keys(data)) {
    if(typeof data[key] == "object")
      obj[key] = parseFloats(data[key])
    else
      obj[key] = parseFloat(data[key])
  }
  return obj
}

class ConfigView extends React.Component {
  constructor(props, context) {
    super(props, context)
    this.state = {
      altitude_rate_pid: {},
      altitude_position_pid: {},
      attitude_roll_stab: {},
      attitude_pitch_stab: {},
      yaw_rate_pid: {},
      yaw_position_pid: {},
    }
    Object.keys(this.state).reduceRight(this.fetchPidConfig.bind(this), (_ => null))()
  }

  fetchPidConfig(nextCallback, field) {
    return (_ => $.getJSON('controllable_vehicle_i/' + field).done(data => {
      this.setState({ [field]: data })
      nextCallback()
    }))
  }

  savePidConfig(field, data) {
    let state = React.addons.update(this.state, { [field]: { $merge: data } })
    this.setState(state)
    $.ajax({
      type: "POST",
      url: 'controllable_vehicle_i/' + field,
      contentType: "application/json",
      data: JSON.stringify(parseFloats(state[field])),
    })
  }

  render() {
    return (
      <div className="container">
      <div className="row">
        <PidConfig {...this.state.altitude_rate_pid} name="Altitude rate" changed={ data => this.savePidConfig('altitude_rate_pid', data) } />
        <PidConfig {...this.state.altitude_position_pid} name="Altitude position" changed={ data => this.savePidConfig('altitude_position_pid', data) } />
        <PidConfig {...this.state.attitude_roll_stab.rate} name="Roll rate" changed={ data => this.savePidConfig('attitude_roll_stab', { rate: data }) } />
        <PidConfig {...this.state.attitude_roll_stab.pos} name="Roll position" changed={ data => this.savePidConfig('attitude_roll_stab', { pos: data }) } />
        <PidConfig {...this.state.attitude_pitch_stab.rate} name="Pitch rate" changed={ data => this.savePidConfig('attitude_pitch_stab', { rate: data }) } />
        <PidConfig {...this.state.attitude_pitch_stab.pos} name="Pitch position" changed={ data => this.savePidConfig('attitude_pitch_stab', { pos: data }) } />
        <PidConfig {...this.state.yaw_rate_pid} name="Yaw rate" changed={ data => this.savePidConfig('yaw_rate_pid', data) } />
        <PidConfig {...this.state.yaw_position_pid} name="Yaw position" changed={ data => this.savePidConfig('yaw_position_pid', data) } />
      </div>
      </div>
    )
  }
}

React.render(<ConfigView/>, document.getElementById('config-root'))
