ErlServer为一个练手项目，参考朋友的项目（https://github.com/mafei198/game_server.git）的 GameServer。
    
run server：(if windows, use git bash please)

  1,update file include/properties.hrl about database part

  2,login in your mysql, just create your databases without tables

  3,\> make

  4,\> make start, make start would init your config by *.conf files and db by db.sql and protocol by protocol.pro 
    or \> make console, only start server

由于个人原因，该项目大部分模块耦合性比较高，也就是说，许多模块之间存在比较复杂的依赖关系，所以，大部分模块不能直接单独独立出来使用，该项目只能算一个server，而非server engine

create.md 记录了完整的项目创建过程和实现细节