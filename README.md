lx_gen_server
=====

基于 `parse_transform` 实现通用代码继承的 `gen_server` 实现.

Build
-----
    $ rebar3 compile

Usage
-----
两种实现方式:

1. lx_gen_server : 类似 `params_module` 参数化模块的做法 
    加入代码 `-include("lx_gen_server_transform.hrl").` , 默认使用 `lx_gen_server.erl`, 若要使用自定义类, 则可以通过`-lx_gen_server(xxx).` 指定类名.

2. lx_gen_server_inner : 将父类的公用方法代码直接编译进目标子类
    加入代码 `-include("lx_gen_server_inner_transform.hrl").` , 默认使用 `lx_gen_server_inner.erl`, 若要使用自定义类, 则可以通过`-lx_gen_server_inner(xxx).` 指定类名.

Attention
-----
**指定的父类文件要优先编译**
