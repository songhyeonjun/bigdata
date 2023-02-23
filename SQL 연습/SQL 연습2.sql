select * from cslee.tab1 t;

SELECT EMP_NAME 사원이름, ORG_CD 소속, POSITION 직무, SALARY 연봉
FROM cslee.TB_EMP
WHERE POSITION ='대리';

-- 현재 시간 조회하는 방법
select now();

-- 현재시간보다 하루 전 날짜 구하는 방법
select now(), now()::date - '1 day' ::interval;

-- 현재 타임존 조회하는 방법
show timezone;

--시스템 일자를 조회하는 방법
select current_date, current_time, timeofday();
select now(), current_timestamp, timestamp 'now';

-- 날짜에서 연도를 추출하는 방법 (date_part, extract, date_trunc 각각 결과가 어떻게 다른지 비교해보세요.)
select date_part('year', timestamp '2020-07-30 20:38:40');
select date_part('year', current_timestamp);
select extract('isoyear' from date '2006-01-01');
select extract('isoyear' from current_timestamp);
select date_trunc('year', timestamp '2020-07-30 20:38:40');
select date_trunc('year', current_timestamp);

-- 날짜에서 월을 추출하는 방법 (date_part, extract, date_trunc 각각 결과가 어떻게 다른지 비교해보세요.)
select date_part('month', timestamp '2020-07-30 20:38:40');

select date_part('month', current_timestamp);
select extract('month' from timestamp '2020-07-30 20:38:40');
select extract('month' from interval '2 years 3 months');
select extract('month' from interval '2 years 13 months');
select date_trunc('month', timestamp '2020-07-30 20:38:40'); -- 월까지만 나오고 나머지는 초기화로 맞추는 방법임

-- 날짜에서 일을 추출하는 방법
select date_part('day', timestamp '2020-07-30 20:38:40');
select date_trunc('day', timestamp '2020-07-30 20:38:40'); -- 일자까지만 나오고 나머지는 초기화로 맞누는 방법임

-- 시간에서 시를 추출하는 방법
select date_part('hour', timestamp '2013-07-30 20:38:40');
select date_part('hour', interval '4 hours 3 minutes');
select date_trunc('hour', timestamp '2020-07-30 20:38:40');

-- 시간에서 분을 추출하는 방법
select date_part('minute', timestamp '2020-07-30 20:38:40');
select date_trunc('minute', timestamp '2020-07-30 20:38:40');

-- 시간에서 초를 추출하는 방법
select date_part('second', timestamp '2013-07-30 20:38:40');
select extract('second' from time '17:12:28.5');
select date_trunc('second', timestamp '2013-07-30 20:38:40');

-- 교재 p.154
-- 주수 구하기
SELECT EMP_NAME, to_char((current_timestamp - ENT_DATE),'W') WEEKS
FROM cslee.TB_EMP
WHERE ORG_CD = '10';

-- to_Char 구문
select emp_name, ent_date,
to_char(ent_date,'yyyy') 입사년,
to_char(ent_date,'MM') 입사월,
to_char(ent_date,'DD') 입사일
from cslee.tb_emp;

-- Extract 구문
select emp_name, ent_date,
extract('year' from ent_date) 입사년,
extract('month' from ent_date) 입사월,
extract('day' from ent_date) 입사일
from cslee.tb_emp;


select extract ('year' from timestamp '0001-01-01 BC');

-- 변환함수(명시적 데이터 유형 변환) to_date, to_number, to_timestamp, cast
select to_date('05 Dec 2000', 'DD Mon YYYY');
select to_number('12,454.8-', '99G999D9S');
select to_timestamp('05 Dec 2000', 'DD Mon YYYY');

-- 변환함수 전체 (예) 교재 p.158
SELECT CAST(123.4 AS VARCHAR(10)), 
CAST('123.5' AS NUMERIC), 
CAST(1234.5678 AS DEC(6,2)), 
CAST(CURRENT_TIMESTAMP AS DATE), 
TO_CHAR(CURRENT_TIMESTAMP, 'YYYY-MM-DD HH24:MI:SS'), 
TO_CHAR(1234.56, 'L9,999.99'), 
TO_NUMBER('$12,345', '$99,999'), 
TO_DATE('2014/03/01 21:30:18','YYYY/MM/DD HH24:MI:SS'), 
TO_TIMESTAMP('2014/03/01 21:30:18','YYYY/MM/DD HH24:MI:SS');

-- Case 표현 예제, 교재 p.158
SELECT emp_name,
case when salary > 50000000
then salary
else 50000000
end as revised_salary
FROM cslee.tb_emp;

-- 교재 p.159 실습
select org_name,
case org_name
when '영업1팀' then '지사'
when '영업2팀' then '지사'
when '영업3팀' then '지사'
when '경영관리팀' then '본사'
else '지점'
end as AREA
from cslee.tb_org;

-- 교재 p.160
-- case 문
SELECT EMP_NAME,
CASE WHEN SALARY >= 90000000 THEN 'HIGH'
WHEN SALARY >= 60000000 THEN 'MID'
ELSE 'LOW'
END AS SALARY_GRADE
FROM cslee.TB_EMP;

-- 중첩 Case문
SELECT EMP_NAME, SALARY,
CASE WHEN SALARY >= 50000000
THEN 20000000
ELSE (CASE WHEN SALARY >= 20000000
THEN 10000000
ELSE SALARY * 0.5
END)
END as BONUS
FROM cslee.TB_EMP;

-- NULL관련 함수 p.162
-- coalesce 함수
SELECT EMP_NAME, position,
-- NVL(POSITION,'없음'), -- oracle null 체크함수
coalesce(POSITION,'없음') -- null 체크함수
from cslee.TB_EMP;

-- case함수
SELECT EMP_NAME, POSITION,
CASE WHEN POSITION IS NULL
THEN '없음'
ELSE POSITION
END AS 직책
FROM cslee.TB_EMP;

--NULL과 공집합 p.163
select coalesce(salary, 0) SAL
from cslee.tb_emp
where emp_name='김태진';

SELECT MAX(salary) SAL
FROM cslee.tb_emp
WHERE emp_name='김태진';

SELECT coalesce(MAX(salary), 9999) SAL
FROM cslee.tb_emp
WHERE emp_name='김태진';

--p. 167
SELECT COUNT(*) "전체건수"
, COUNT(POSITION) "직책건수"
, COUNT(DISTINCT POSITION) "직책종류"
, MAX(SALARY) "최대"
, MIN(SALARY) "최소"
, AVG(SALARY) "평균"
FROM cslee.TB_EMP;

-- Group by 절
-- p. 169
SELECT POSITION "직책",
COUNT(*) "인원수",
MIN(SALARY) "최소",
MAX(SALARY) "최대",
AVG(SALARY) "평균"
FROM cslee.TB_EMP
GROUP BY POSITION;

-- Having 절
SELECT ORG_CD "부서",
COUNT(*) "인원수",
AVG( SALARY) "평균"
FROM cslee.TB_EMP
GROUP BY ORG_CD
HAVING COUNT(*) >= 4;

SELECT ORG_CD "부서",
MAX(SALARY) "최대"
FROM cslee.TB_EMP
GROUP BY ORG_CD
HAVING MIN(SALARY) <= 40000000;

SELECT ORG_CD, POSITION, AVG( SALARY)
FROM cslee.TB_EMP
WHERE POSITION IN ('과장','대리','사원')
GROUP BY ORG_CD, POSITION;

SELECT ORG_CD
, AVG(CASE POSITION WHEN '과장' THEN SALARY END) "과장"
, AVG(CASE POSITION WHEN '대리' THEN SALARY END) "대리"
, AVG(CASE POSITION WHEN '사원' THEN SALARY END) "사원"
FROM cslee.TB_EMP
WHERE POSITION IN ('과장','대리','사원')
GROUP BY ORG_CD;

SELECT ORG_CD,
sum(COALESCE((case position WHEN '팀장' THEN 1 ELSE 0 END),0)) "팀장",
SUM(COALESCE((case position WHEN '과장' THEN 1 ELSE 0 END),0)) "과장",
SUM(COALESCE((case position WHEN '대리' THEN 1 ELSE 0 END),0)) "대리",
SUM(COALESCE((case position WHEN '사원' THEN 1 ELSE 0 END),0)) "사원"
from cslee.tb_emp
group BY ORG_CD;

SELECT ORG_CD,
COALESCE (SUM(case position WHEN '팀장' THEN 1 END),0) “팀장”,
COALESCE (SUM(case position WHEN '과장' THEN 1 END),0) “과장”,
COALESCE (SUM(case position WHEN '대리' THEN 1 END),0) “대리”,
COALESCE (SUM(case position WHEN '사원' THEN 1 END),0) “사원”
from cslee.TB_EMP
GROUP BY ORG_CD;

-- Equi Join 실습
-- [예제] 사원 테이블과 조직 테이블의 조인 SQL
SELECT TB_EMP.EMP_NAME, TB_EMP.ORG_CD
, TB_ORG.ORG_CD, TB_ORG.ORG_NAME
FROM cslee.TB_EMP, cslee.TB_ORG
WHERE cslee.TB_EMP.ORG_CD = cslee.TB_ORG.ORG_CD;

-- [예제] 사원이름, 소속부서코드, 부서명(조직명), 직책을 출력하시오
SELECT TB_EMP.EMP_NO,
TB_EMP.EMP_NAME,
TB_EMP.ORG_CD,
TB_ORG.ORG_NAME,
TB_EMP.POSITION
FROM cslee.TB_EMP,
cslee.TB_ORG
WHERE cslee.TB_EMP.ORG_CD = cslee.TB_ORG.ORG_CD; -- 조인컬럼

-- Alias 사용
SELECT E.EMP_NO,
E.EMP_NAME,
E.ORG_CD,
O.ORG_NAME,
E.POSITION
FROM cslee.TB_EMP AS E,
cslee.TB_ORG AS O
WHERE E.ORG_CD = O.ORG_CD -- 조인컬럼 지정
AND E.POSITION = '팀장' -- 필터조건
ORDER BY O.ORG_NAME; -- 정렬

-- [예제] 계좌번호, 고객명, 상품명, 계약금액, 관리자명을 출력한다.
SELECT A.ACCNO,
C.CUST_NAME,
P.PROD_NAME,
A.CONT_AMT,
E.EMP_NAME
FROM cslee.TB_ACCNT A, cslee.TB_CUST C, cslee.TB_PROD P, cslee.TB_EMP E
WHERE A.CUST_NO = C.CUST_NO
AND A.PROD_CD = P.PROD_CD
AND A.MANAGER = E.EMP_NO;

-- [예제]사원별 급여와 어느 등급에 속하는지 알고 싶다는 요구사항에 대한 Non EQUI JOIN
SELECT E.EMP_NAME 사원명,
E.SALARY 연봉,
S.GRADE 급여등급
FROM cslee.TB_EMP E, cslee.TB_GRADE S
where E.SALARY
BETWEEN S.LOW_SAL AND S.HIGH_SAL;

-- 표준 조인(ANSI 표현법, ANSI 표현법으로 익히는 것이 좋음)
-- Inner Join
--[예제] 사원 번호와 사원 이름, 소속부서 코드와 소속부서 이름을 찾아본다.
--(1) WHERE 절 JOIN 조건
SELECT EMP.EMP_NO, EMP.EMP_NAME, ORG.ORG_NAME
FROM cslee.TB_EMP EMP, cslee.TB_ORG ORG
WHERE EMP.ORG_CD = ORG.ORG_CD;

--(2) FROM 절 JOIN 조건
SELECT EMP.EMP_NO, EMP.EMP_NAME, ORG.ORG_NAME
FROM cslee.TB_EMP EMP
INNER JOIN cslee.TB_ORG ORG
ON EMP.ORG_CD = ORG.ORG_CD;

--(3) INNER 키워드 생략
SELECT EMP.EMP_NO, EMP.EMP_NAME, ORG.ORG_NAME
FROM cslee.TB_EMP EMP
JOIN cslee.TB_ORG ORG
ON EMP.ORG_CD = ORG.ORG_CD;

-- FROM 절 JOIN 조건
--[예제] 계좌 테이블에서 계좌번호와, 고객번호, 고객의 명을 고객테이블과 조인하여 찾아본다.
SELECT ACC.ACCNO, ACC.CUST_NO, CUST.CUST_NAME
FROM cslee.TB_ACCNT ACC
INNER JOIN cslee.TB_CUST CUST
ON (CUST.CUST_NO = ACC.CUST_NO);

--FROM 절 JOIN 조건
--[예제] 조직코드 10인 부서의 소속 사원 이름 및 소속 부서 코드, 부서 코드, 부서 이름을 찾아본다.
SELECT E.EMP_NAME, E.ORG_CD, O.ORG_CD, O.ORG_NAME
FROM cslee.TB_EMP E
JOIN cslee.TB_ORG O
ON (E.ORG_CD = O.ORG_CD)
WHERE E.ORG_CD = '10';

-- ON조건절- 다중조인
-- (1) WHERE 절 JOIN 조건
SELECT A.ACCNO 계좌번호, C.CUST_NAME 고객명, P.PROD_NAME 상품명
, A.CONT_AMT 계약금액, E.EMP_NAME 담당자명
FROM cslee.TB_ACCNT A, cslee.TB_CUST C, cslee.TB_PROD P, cslee.TB_EMP E
WHERE A.CUST_NO = C.CUST_NO
AND A.PROD_CD = P.PROD_CD
AND A.MANAGER = E.EMP_NO;

-- (2) ON 절 JOIN 조건
SELECT A.ACCNO 계좌번호, C.CUST_NAME 고객명, P.PROD_NAME 상품명
, A.CONT_AMT 계약번호, E.EMP_NAME 담당자명
FROM cslee.TB_ACCNT A
INNER JOIN cslee.TB_CUST C ON A.CUST_NO = C.CUST_NO
INNER JOIN cslee.TB_PROD P ON A.PROD_CD = P.PROD_CD
INNER JOIN cslee.TB_EMP E ON A.MANAGER = E.EMP_NO;

-- CROSS 조인(몇개의 데이터가 나올지 미리 예측해보세요)
select count(emp_name) from cslee.tb_emp;

select count(org_name) from cslee.tb_org;

SELECT E.EMP_NAME, O.ORG_NAME
FROM cslee.TB_EMP E CROSS JOIN cslee.TB_ORG O
ORDER BY EMP_NAME;

select count(e.emp_name)
FROM cslee.TB_EMP E CROSS JOIN cslee.TB_ORG O;

-- [예제] 직원중에 아직 부서배정이 안된 사원도 있다. 사원(TB_EMP)과 조직(TB_ORG)을 JOIN하되 부서배정이
-- 안된 사원의 정보도 같이 출력하도록 한다.
SELECT E.EMP_NO 사번, E.EMP_NAME 사원명,
E.POSITION 직책, O.ORG_NAME 부서명
FROM cslee.TB_EMP E LEFT OUTER JOIN cslee.TB_ORG O
ON E.ORG_CD = O.ORG_CD
WHERE E.POSITION = '사원';

-- Right Outer Join
--[예제] 이전 예제에서 LEFT JOIN을 OUTER JOIN으로 표현한다.
SELECT E.EMP_NO 사번, E.EMP_NAME 사원명, E.POSITION 직책, O.ORG_NAME 부서명
FROM cslee.TB_ORG O RIGHT OUTER JOIN cslee.TB_EMP E
ON E.ORG_CD = O.ORG_CD
WHERE E.POSITION = '사원';

-- Full Outer Join
SELECT A.ORG_CD, A.ORG_NAME, B.ORG_CD, B.ORG_NAME
FROM cslee.TB_ORG A FULL OUTER JOIN cslee.TB_ORG B
ON A.ORG_CD=B.ORG_CD;

SELECT A.ORG_CD, A.ORG_NAME, B.ORG_CD, B.ORG_NAME
FROM cslee.TB_ORG A LEFT OUTER JOIN cslee.TB_ORG B
ON A.ORG_CD = B.ORG_CD
UNION
SELECT A.ORG_CD, A.ORG_NAME, B.ORG_CD, B.ORG_NAME
from cslee.TB_ORG A RIGHT OUTER JOIN cslee.TB_ORG B
ON A.ORG_CD = B.ORG_CD;