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
->6->CREATE TABLE `alliance` (
  `uuid` varchar(16) NOT NULL DEFAULT '',
  `name` varchar(50) NOT NULL DEFAULT '',
  `level` int(11) unsigned NOT NULL DEFAULT '1',
  `coins` int(11) unsigned NOT NULL DEFAULT '0',
  `president` varchar(16) NOT NULL DEFAULT '',
  `officers` varchar(255) DEFAULT NULL,
  `elites` varchar(255) DEFAULT NULL,
  `members` text NOT NULL,
  PRIMARY KEY (`uuid`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
->7->alter table `user` add column `alliance_id` varchar(16) DEFAULT NULL;