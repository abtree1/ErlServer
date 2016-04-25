ErlServer为一个练手项目，参考朋友的项目（https://github.com/mafei198/game_server.git）的GameServer，本文件记录整个项目创建过程
   rebar 可通过github获取
1. 通过rebar创建项目
	./rebar create-app appid=erl_server

2. 编写makefile来启动项目

3. 为了方便日志记录
   尝试使用sasl，但sasl需要控制台命令rb才能处理日志，极为不方便
   因此引入lager库

   lager库需要在erl_server.config中配置，并且在启动时引入该config
   		  需要在rebar.config中设置{erl_opts, [{parse_transform, lager_transform}]}
   		  最后lager:start()

	在rebar.config中加入lager及其依赖库的依赖
  	同时在编译时./rebar get-deps编译依赖库

4.读取配置文件，并转换为record
  1,配置项保存格式[{key, value, value,...},{key, value, value,...}...]
  2,不同的配置表保存在ets里
  	key = 表名（term）
  	value = 1的配置项
  3,每个配置项对应一个与表明相同的record，在读取的时候会自动转换为相应的record
  	record信息保存在config.hrl中，after_server_start后，该文件自动生成
  4,考虑放弃ets表，将数据存入config_data.hrl中
    
    config文件必须满足的格式：
        由于erlang语言的特性，config文件最好不使用大写字母,至少首字母不能大写
        config文件必须以".conf"结尾
        config文件内容如模版，值得注意的是，关键字符包括 TAB(\t) 用于项之间分割，因此，配置内容里面不能有TAB符号，换行符(\r\n or \n)，用于列之间分割，因此，配置中不能出现，冒号(:)，用于title与其类型分割，在title中不能重复出现，类型目前只有string，integer，float三种。
        另外，第一列标示符，＃号，表示该列有效，不带表示该列无效，title中的flag可有可无或是其它占位符，但其后的TAB(\t)不可省略。

5.重新生成的hrl文件需要重新编译，这里通过修改makefile，增加start选项，使server在运行之前先编译config

6.dirty words 为了加快其遍历速度，将其作为特殊配置文件处理，采用字典树。其配置文件dirtywords.filter，换行符可以是"\r\n" 或 "\n"
