import Lake
open Lake DSL

require «nest-core» from git "https://github.com/hargonix/nest-core" @ "main"

package «nest-unit» {
  -- add package configuration options here
}

@[default_target]
lean_lib «NestUnit» {
  -- add library configuration options here
}

lean_exe Main {

}
