1->CREATE TABLE `user` (
  `uuid` varchar(16) NOT NULL DEFAULT '',
  `name` varchar(26) NOT NULL DEFAULT '',
  `level` int(11) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`uuid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
->2->CREATE TABLE `item` (
  `uuid` varchar(16) NOT NULL DEFAULT '',
  `name` varchar(255) NOT NULL DEFAULT '',
  `amount` int(11) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`uuid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
->3->alter table `item` add column `type` smallint(5) unsigned NOT NULL DEFAULT '0';
->4->alter table `user` add column `account` varchar(50) NOT NULL DEFAULT '', add column `passwd` varchar(100) NOT NULL DEFAULT '';
->5->CREATE UNIQUE INDEX `account` on `user`('account');