create table user_account (
	id varchar(10) not null, 
	username varchar(30) unique,
	password varchar(20), 
	address varchar(70), 
	firstname varchar(20), 
	lastname varchar(20),
	type varchar(15), 
	passportnumber varchar(30), 
	constraint pk_useraccount_id primary key (id)
	);

create table student (
	id varchar(10) not null, 
	birthdate date, 
	achieved_educational_degrees varchar(200), 
	occupation varchar(20), 
	constraint pk_student_id primary key (id), 
	constraint fk_student_id foreign key (id) references user_account on update cascade on delete cascade
	);



create table attends (
	id varchar(10) not null, 
	course_number varchar(20) not null, 
	constraint pk_attends primary key (id, course_number), 
	constraint fk_attends_id foreign key (id) references student on update cascade on delete cascade, 
	constraint fk_attends_course_number foreign key (course_number) references course on update cascade on delete cascade
	);



create table attends_comments (
	id varchar(10) not null,
	course_number varchar(20) not null, 
	comment varchar(200) not null, 
	constraint pk_attends_comments primary key (id, course_number, comment), 
	constraint fk_attends_comments_id foreign key (id) references attends on update cascade on delete cascade, 
	constraint fk_attends_comments_course_number foreign key (course_number) references attends on update cascade on delete cascade
	);



create table student_submits (
	student_id varchar(10) not null,
	course_number varchar(20) not null, 
	instructor_id varchar(10) not null, 
	report varchar(200), 
	constraint pk_student_submits primary key (student_id, course_number, instructor_id), 
	constraint fk_student_submits_student_id foreign key (student_id) references student on update cascade on delete cascade,
        constraint fk_student_submits_course_number foreign key (course_number) references course on update cascade on delete cascade, 
	constraint fk_student_submits_instructor_id foreign key (instructor_id) references instructor on update cascade on delete cascade
	);


create table employee (
	id varchar(10) not null,
        hired_date date,
	career_start_date date,
	job_description varchar(200),
	constraint pk_employee_id primary key (id),
	constraint fk_employee_id foreign key (id) references user_account on update cascade on delete cascade
	);

create table instructor (
	id varchar(10) not null,
	author_identifier integer not null,
	academic_degrees varchar(200),
	working_rate_per_hour real,
	mod_id varchar(10), 
	constraint pk_instrustor_id primary key (id),
	constraint fk_instructor_id foreign key (id) references employee on update cascade on delete cascade,
	constraint fk_instructor_mod_id foreign key (mod_id) references moderator on update cascade on delete cascade
	);

create table moderator (
	id varchar(10) not null,	
	date_of_acquiring date,
	degree_name varchar(30),
	constraint pk_moderator_id primary key (id),
	constraint fk_moderator_id foreign key (id) references employee on update cascade on delete cascade
	);

create table user_phone (
	id varchar(10) not null,
	phone varchar(10) not null,
	constraint pk_user_phone primary key (id,phone),
	constraint fk_user_phone_id foreign key (id) references user_account on update cascade on delete cascade 
	);

create table user_email (
	id varchar(10) not null,
	email varchar(10) not null,
	constraint pk_user_email primary key (id,email),
	constraint fk_user_email_id foreign key (id) references user_account on update cascade on delete cascade
	);

create table course (
	course_number varchar(20) not null,
	title varchar(20),
	level varchar(15),
	duration time,
	inst_id varchar(10),
	constraint pk_course_course_number primary key (course_number),
	constraint pk_course_inst_id foreign key (inst_id) references instructor on update cascade on delete cascade
	);

create table moderator_submits (
	course_number varchar(20) not null,
	inst_id varchar(10) not null,
	moderator_id varchar(10) not null,
	report varchar(200),
	constraint pk_moderator_submits primary key (course_number,inst_id,moderator_id),
	constraint fk_moderator_submits_course_number foreign key (course_number) references course on update cascade on delete cascade,
	constraint fk_moderator_submits_inst_id foreign key (inst_id) references instructor on update cascade on delete cascade,
	constraint fk_moderator_submits_moderator_id foreign key (moderator_id) references moderator on update cascade on delete cascade
	);

create table exam (
	exam_number varchar(10) not null,
	course_number varchar(20) not null,
	no_of_question integer ,
	time time,
	date date,
	constraint pk_exam_exam_number primary key (exam_number,course_number),
	constraint fk_exam_course_number foreign key (course_number) references course on update cascade on delete cascade
	);

create table topic (
	topic_id varchar(10) not null,
	title varchar(20),
	description varchar(200),
	constraint pk_topic_topic_id primary key (topic_id)
	);

create table contains (
	topic_id varchar(10) not null,
	course_number varchar(20) not null,
	constraint pk_contains primary key (topic_id,course_number),
	constraint fk_contains_topic_id foreign key (topic_id) references topic on update cascade on delete cascade,
	constraint fk_contains_course_number foreign key (course_number) references course on update cascade on delete cascade
	);

create table tutorial (
	tut_id varchar(10) not null,
	topic_id varchar(10) not null,
	constraint pk_tutorial primary key (tut_id,topic_id),
	constraint fk_tutorial_topic_id foreign key (topic_id) references topic on update cascade on delete cascade
	);

create table material_item (
	filename varchar(20) not null,
	tut_id varchar(10) not null,
	topic_id varchar(10) not null,
	constraint pk_material_item primary key (filename,tut_id,topic_id),
	constraint fk_material_item_tut_id foreign key (tut_id) references tutorial on update cascade on delete cascade,
	constraint fk_material_item_topic_id foreign key (topic_id) references tutorial on update cascade on delete cascade
	);

create table project (
	filename varchar(20) not null,
	tut_id varchar(10) not null,
	topic_id varchar(10) not null,
	prob_description varchar(200),
	requirements varchar(200),
	tools varchar(200),
	constraint pk_project primary key (filename,tut_id,topic_id),
	constraint fk_project_tut_id foreign key (tut_id) references material_item on update cascade on delete cascade,
	constraint fk_project_topic_id foreign key (topic_id) references material_item on update cascade on delete cascade,
	constraint fk_project_filename foreign key (filename) references material_item on update cascade on delete cascade
	);

create table presentation (
	filename varchar(20) not null,
	tut_id varchar(10) not null,
	topic_id varchar(10) not null,
	subject varchar(30),
	constraint pk_presentation primary key (filename,tut_id,topic_id),
	constraint fk_presentation_tut_id foreign key (tut_id) references material_item on update cascade on delete cascade,
	constraint fk_presentation_topic_id foreign key (topic_id) references material_item on update cascade on delete cascade,
	constraint fk_presentation_filename foreign key (filename) references material_item on update cascade on delete cascade
	);

create table assignment (
	filename varchar(20) not null,
	tut_id varchar(10) not null,
	topic_id varchar(10) not null,
	no_of_exercises integer,
	description varchar(200),
	constraint pk_assignment primary key (filename,tut_id,topic_id),
	constraint fk_assignment_tut_id foreign key (tut_id) references material_item on update cascade on delete cascade,
	constraint fk_assignment_topic_id foreign key (topic_id) references material_item on update cascade on delete cascade,
	constraint fk_presentation_filename foreign key (filename) references material_item on update cascade on delete cascade
	);

create table question (
	question_number integer not null,
	topic_id varchar(10) not null,
	text varchar(300) not null,
	solution_time time,
	no_of_marks real,
	constraint pk_question primary key (question_number,topic_id),
	constraint fk_question_topic_id foreign key (topic_id) references topic on update cascade on delete cascade
	);

create table exam_consists (
	question_number inetger not null,
	topic_id varchar(10) not null,
	exam_number varchar(10) not null,
	course_number varchar(20) not null,
	constraint pk_exam_consists primary key (question_number,exam_number,course_number,topic_id),
	constraint fk_exam_consists_question_number foreign key (question_number) references question on update cascade on delete cascade,
	constraint fk_exam_consists_topic_id foreign key (topic_id) references question on update cascade on delete cascade,
	constraint fk_exam_consists_exam_number foreign key (exam_number) references exam on update cascade on delete cascade,
	constraint fk_exam_consists_course_number foreign key (course_number) references exam on update cascade on delete cascade
	);

create table assignment_consists (
	question_number integer not null,
	filename varchar(20) not null,
	tut_id varvhar(10) not null,
	ass_topic_id varchar(10) not null,
	ques_topic_id varchar(10) not null,
	constraint pk_assignment_consists primary key (question_number,filename,tut_id,ass_topic_id,ques_topic_id),
	constraint fk_assignment_consists_question_number foreign key (question_number) references question on update cascade on delete cascade,
	constraint fk_assignment_consists_ques_topic_id foreign key (ques_topic_id) references question on update cascade on delete cascade,
	constraint fk_assignment_consists_filename foreign key (filename) references assignment on update cascade on delete cascade,
	constraint fk_assignment_consists_tut_id foreign key (tut_id) references assignment on update cascade on delete cascade,
	constraint fk_assignment_consists_ass_topic_id foreign key (ass_topic_id) references assignment on update cascade on delete cascade
	);

create table external_resources (
	id varchar(10) not null,
	author_identifier integer not null,
	constraint pk_external_resources primary key (id)
	);

create table book (
	isbn varchar(10) not null,
	id varchar(10) not null,
	edition varchar(5),
	constraint pk_book primary key (isbn,id),
	constraint fk_book_id foreign key (id) references external_resources on update cascade on delete cascade
	);

create table book_author (
	book_isbn varchar(10) not null,
	book_id varchar(10) not null,
	author varchar(30) not null,
	constraint pk_book_author primary key (book_isbn,book_id,author),
	constraint fk_book_author_book_isbn foreign key (book_isbn) references book on update cascade on delete cascade,
	constraint fk_book_author_book_id foreign key (book_id) references book on update cascade on delete cascade
	);

create table website (
	url varchar(150) not null,
	id varchar(10) not null,
	constraint pk_website primary key (url,id),
	constraint fk_website_id foreign key (id) references external_resources on update cascade on delete cascade
	);

create table publication (
	pub_number varchar(10) not null,
	id varchar(10) not null,
	title varchar(20) not null,
	date_of_publication date,
	constraint pk_publication primary key (pub_number,id),
	constraint fk_publication_id foreign key (id) references external_resources on update cascade on delete cascade
	);

create table publication_author (
	pub_number varchar(10) not null,
	pub_id varchar(10) not null,
	author varchar(20) not null,
	constraint pk_publication_author primary key(pub_number,pub_id,author),
	constraint fk_publication_author_pub_number foreign key (pub_number) references publication on update cascade on delete cascade,
	constraint fk_publication_author_pub_id foreign key (pub_id) references publication on update cascade on delete cascade
	);

create table author (
	author_identifier integer not null,
	constraint pk_author primary key(author_identifier)
	);

create table author_creates (
	author_identifier integer not null,
	filename varchar(20) not null,
	tut_id varchar(10) not null,
	topic_id varchar(10) not null,
	constraint pk_author_creates primary key (author_identifier,filename,tut_id,topic_id),
	constraint fk_author_creates_author_identifier foreign key (author_identifier) references author on update cascade on delete cascade,
	constraint fk_author_creates_filename foreign key (filename) references material_item on update cascade on delete cascade,
	constraint fk_author_creates_tut_id foreign key (tut_id) references material_item on update cascade on delete cascade,
	constraint fk_author_creates_topic_id foreign key (topic_id) references material_item on update cascade on delete cascade
	);
				
-- inserting the items into the tables --

INSERT INTO user_account
	VALUES('1234', 'fredmorcos', 'fredfredfred', 'freds home', 'fred', 'morcos', 'administrator', 'fm1234');
INSERT INTO user_account
	VALUES('5678', 'nardinebasta', 'narnarnar', 'nardines home', 'nardine', 'basta', 'student', 'nb5678');
INSERT INTO user_account
	VALUES('7890', 'christinemaher', 'koukoukou', 'christines home', 'christine', 'maher', 'moderator', 'cm7890');
INSERT INTO user_account
	VALUES('3456', 'ahmedshalankah', 'ahmedahmedahmed', 'ahmeds home', 'ahmed', 'shalankah', 'student', 'ahmed466783');
INSERT INTO user_account
	VALUES('4353', 'marcmazloum', 'marcmarcmarc', 'marcs home', 'marc', 'mazloum', 'student', 'marc873465');
INSERT INTO user_account
	VALUES('3454', 'hodanaguib', 'hodahodahoda', 'hodas home', 'hoda', 'naguib', 'instructor', 'sdf234897');
INSERT INTO user_account
	VALUES('3344', 'slim', 'slimslimslim', 'slims home', 'slim', 'abdennadher', 'instructor', 'sabn278367');

INSERT INTO instructor
	VALUES('5556', 2, 'bachelor', 8.7, '7890');

INSERT INTO course
	VALUES('PHY123', 'Physics', 'Hard', null, '3454');
INSERT INTO course
	VALUES('MATH456', 'Math', 'Normal', null, '3344');
INSERT INTO course
	VALUES('CS986', 'CS', 'Hard', null, '');
INSERT INTO course
	VALUES('ENG777', 'English', 'Easy', null, '');
