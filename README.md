ErlServer为一个练手项目，参考朋友的项目（https://github.com/mafei198/game_server.git）的 GameServer。
    
run server：(if windows, use git bash please)

  1,update file include/properties.hrl about database part

  2,login in your mysql, just create your databases without tables

  3,\> make

  4,\> make start:
  	   make start would init your config by *.conf files and db by db.sql and protocol by protocol.pro
  	   make start would build map(hexagon map or rectangle map) by random with control
  
  5,\> make console, only start server

  6,\> make config, auto init config data by *.conf 

  7,\> make db, auto migrate database without create database, but create tables and alter tables by db.sql

  8,\> make proto, auto init protocol by protocol.pro

  9,\> make map, random build map (hexagon map or rectangle map) with rules

Notice: all db table must have column uuid as PRIMARY KEY

Notice: map: 1 group = 100 grids, 1 section = 100 groups, 1 group grids's level and type distribute random with config control

由于个人原因，该项目大部分模块耦合性比较高，也就是说，许多模块之间存在比较复杂的依赖关系，所以，大部分模块不能直接单独独立出来使用，该项目只能算一个server，而非server engine

create.md 记录了完整的项目创建过程和实现细节