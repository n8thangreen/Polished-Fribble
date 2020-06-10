-- phpMyAdmin SQL Dump
-- version 5.0.2
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1:3306
-- Generation Time: Jun 09, 2020 at 09:42 PM
-- Server version: 8.0.20
-- PHP Version: 7.2.24-0ubuntu0.18.04.6

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `room_avail`
--
CREATE DATABASE IF NOT EXISTS `room_avail` DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
USE `room_avail`;

-- --------------------------------------------------------

--
-- Table structure for table `individual_information`
--

CREATE TABLE `individual_information` (
  `UserName` varchar(20) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
  `Password` varchar(9) DEFAULT NULL,
  `RoomNumber` int DEFAULT NULL,
  `Permissions` char(20) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Dumping data for table `individual_information`
--

INSERT INTO `individual_information` (`UserName`, `Password`, `RoomNumber`, `Permissions`) VALUES
('Admin', 'Password', 0, 'admin'),
('Simon', 'Password1', 1, 'standard'),
('Elinor', 'Password2', 2, 'standard'),
('Richard', 'Password3', 3, 'standard'),
('Gianluca', 'Password4', 4, 'standard');

-- --------------------------------------------------------

--
-- Table structure for table `new_room_status`
--

CREATE TABLE `new_room_status` (
  `Date` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `Weekday` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `Room_no` int NOT NULL,
  `9am_10am` char(20) NOT NULL,
  `10am_11am` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `11am_12pm` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `12pm_1pm` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `1pm_2pm` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `2pm_3pm` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `3pm_4pm` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `4pm_5pm` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

--
-- Dumping data for table `new_room_status`
--

INSERT INTO `new_room_status` (`Date`, `Weekday`, `Room_no`, `9am_10am`, `10am_11am`, `11am_12pm`, `12pm_1pm`, `1pm_2pm`, `2pm_3pm`, `3pm_4pm`, `4pm_5pm`) VALUES
('2020-01-27', 'Mon', 1, 'Available', 'Available', 'Available', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-01-28', 'Tue', 1, 'Unavailable', 'Unavailable', 'Unavailable', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-01-29', 'Wed', 1, 'Available', 'Available', 'Available', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-01-30', 'Thu', 1, 'Available', 'Available', 'Available', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable'),
('2020-02-01', 'Sat', 1, 'Available', 'Available', 'Available', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-02-02', 'Sun', 1, 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable'),
('2020-02-03', 'Mon', 1, 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable'),
('2020-02-04', 'Tue', 1, 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable', 'Unavailable'),
('2020-02-05', 'Wed', 1, 'Unavailable', 'Unavailable', 'Unavailable', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-02-06', 'Thu', 1, 'Unavailable', 'Unavailable', 'Unavailable', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-02-14', 'Fri', 1, 'Unavailable', 'Unavailable', 'Unavailable', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-02-21', 'Fri', 1, 'Unavailable', 'Unavailable', 'Unavailable', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-02-28', 'Fri', 1, 'Unavailable', 'Unavailable', 'Unavailable', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-01-31', 'Fri', 2, 'Booked', 'Booked', 'Booked', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-01-31', 'Fri', 3, 'Available', 'Booked', 'Booked', 'Available', 'Available', 'Available', 'Available', 'Available'),
('2020-02-03', 'Mon', 3, 'Booked', 'Booked', 'Booked', 'Booked', 'Booked', 'Booked', 'Booked', 'Booked');

-- --------------------------------------------------------

--
-- Table structure for table `room_booked`
--

CREATE TABLE `room_booked` (
  `booking_no` int NOT NULL,
  `date` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `time` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `room_no` int NOT NULL,
  `booker` char(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

--
-- Indexes for dumped tables
--

--
-- Indexes for table `new_room_status`
--
ALTER TABLE `new_room_status`
  ADD PRIMARY KEY (`Room_no`,`Date`);

--
-- Indexes for table `room_booked`
--
ALTER TABLE `room_booked`
  ADD PRIMARY KEY (`booking_no`);
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
