%% -*- mode: erlang; -*-
{application, onviso,
 [
  {description, ""},
  {vsn, "0.1.0"}, 
  {modules, ['cli','cli_util','config_generator','onviso','onviso_example','onviso_file_helper','onviso_monitor','onviso_server','overload_handler','wxi']},
  {registered, []},
  {applications, [kernel, stdlib, runtime_tools, inviso]},
  {build_dependencies, []},
  {env, []}
 ]}.
