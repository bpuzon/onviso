%% -*- mode: erlang; -*-
{application, elevator,
 [
  {description, ""},
  {vsn, "1"}, 
  {modules, ['display','e_graphic','elevator','elevators','elev_sup','g_sup','scheduler','sim_sup','stoplist','sys_event','system_sup','tracer','util']},
  {registered, []},
  {applications, [kernel, stdlib]},
  {build_dependencies, []},
  {env, [{filename,"Log.txt"}]}
 ]}.
