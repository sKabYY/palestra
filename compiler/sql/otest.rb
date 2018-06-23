#!/usr/bin/env ruby

require 'pp'

module TestOracle
  puts "start #{self}"
  require './oracle_analyser'
  code = <<EOF
create table MESSAGE_NOTIFY_TASK
(
   PK_ID                varchar2(32)         not null,
   FLIGHT_ID            varchar2(32)         not null,
   MSG_TYPE             varchar2(32)         not null,
   MSG_CONTENT          varchar2(1024),
   PLAN_SEND_TIME       date                 not null,
   constraint PK_MESSAGE_NOTIFY_TASK primary key (PK_ID)
);
EOF
  ast = Oracle::parse(code)
  pp ast.to_tree

  pp Oracle::analyse(code)
end
