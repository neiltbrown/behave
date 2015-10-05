{application, behave, [
	{description, ""},
	{vsn, "rolling"},
	{modules, ['behave_app','behave_sup','mm_game','mm_llfm_game']},
	{registered, [behave_sup]},
	{applications, [kernel,stdlib]},
	{mod, {behave_app, []}}
]}.