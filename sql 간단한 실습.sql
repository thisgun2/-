CREATE DATABASE 병원;
USE 병원;
CREATE TABLE 부서 (
    부서명 VARCHAR(10) PRIMARY KEY,
    부서장 VARCHAR(10),
    부서건물 VARCHAR(10)
);
CREATE TABLE 의사 (
    의사번호 VARCHAR(10) PRIMARY KEY,
    의사이름 VARCHAR(10),
    부서명 VARCHAR(10),
    연봉 INT,
    FOREIGN KEY (부서명) REFERENCES 부서(부서명)
);
CREATE TABLE 환자 (
    환자번호 VARCHAR(10) PRIMARY KEY,
    환자이름 VARCHAR(10),
    생년월일 VARCHAR(10)
);
CREATE TABLE 질병 (
    질병코드 VARCHAR(10) PRIMARY KEY,
    질병명 VARCHAR(10),
    보험청구여부 BOOLEAN
);
CREATE TABLE 진료 (
    진료코드 VARCHAR(10) PRIMARY KEY,
    환자번호 VARCHAR(10),
    의사번호 VARCHAR(10),
    질병코드 VARCHAR(10),
    청구금액 INT,
    FOREIGN KEY (환자번호) REFERENCES 환자(환자번호),
    FOREIGN KEY (의사번호) REFERENCES 의사(의사번호),
    FOREIGN KEY (질병코드) REFERENCES 질병(질병코드)
);
INSERT INTO 부서 VALUES('이비인후과','박동주','A동');
INSERT INTO 부서 VALUES('정형외과','홍지민','B동');
INSERT INTO 부서 VALUES('산부인과','정원일','B동');
INSERT INTO 부서 VALUES('신경외과','한경도','C동');
INSERT INTO 부서 VALUES('치과','박소영','D동');
INSERT INTO 의사 VALUES('A1','박동주','이비인후과',100000000);
INSERT INTO 의사 VALUES('A2','이주은','이비인후과',80000000);
INSERT INTO 의사 VALUES('A3','홍지민','정형외과',100000000);
INSERT INTO 의사 VALUES('A4','유세빈','정형외과',40000000);
INSERT INTO 의사 VALUES('A5','정원일','산부인과',100000000);
INSERT INTO 의사 VALUES('A6','염세빈','산부인과',60000000);
INSERT INTO 의사 VALUES('A7','한경도','신경외과',90000000);
INSERT INTO 의사 VALUES('A8','윤수인','신경외과',30000000);
INSERT INTO 의사 VALUES('A9','박소영','치과',80000000);
INSERT INTO 의사 VALUES('A10','김한나','치과',20000000);
INSERT INTO 환자 VALUES('B1','이건','00년6월28일');
INSERT INTO 환자 VALUES('B2','김건','01년6월28일');
INSERT INTO 환자 VALUES('B3','박건','02년6월28일');
INSERT INTO 환자 VALUES('B4','임건','03년6월28일');
INSERT INTO 환자 VALUES('B5','송건','04년6월28일');
INSERT INTO 환자 VALUES('B6','차건','05년6월28일');
INSERT INTO 환자 VALUES('B7','박건','06년6월28일');
INSERT INTO 질병 VALUES('C1','목감기',TRUE);
INSERT INTO 질병 VALUES('C2','코감기',TRUE);
INSERT INTO 질병 VALUES('C3','손가락골절',FALSE);
INSERT INTO 질병 VALUES('C4','발가락골절',TRUE);
INSERT INTO 질병 VALUES('C5','출산',FALSE);
INSERT INTO 질병 VALUES('C6','신경막손상',TRUE);
INSERT INTO 질병 VALUES('C7','임플란트',TRUE);
INSERT INTO 진료 VALUES('D1','B1','A2','C1',10000);
INSERT INTO 진료 VALUES('D2','B1','A2','C2',20000);
INSERT INTO 진료 VALUES('D3','B2','A1','C1',30000);
INSERT INTO 진료 VALUES('D4','B3','A3','C3',10000);
INSERT INTO 진료 VALUES('D5','B4','A9','C7',20000);
INSERT INTO 진료 VALUES('D6','B5','A5','C5',30000);
INSERT INTO 진료 VALUES('D7','B6','A4','C4',10000);
INSERT INTO 진료 VALUES('D8','B7','A8','C6',20000);

/*FROM 절에 2개의 릴레이션을 사용하는 질의, group by절과 having절을 사용하는 질의*/
/*부서 테이블과 의사 테이블을 조인하고 평균 연봉이 7,000,000 이상인 부서명과 평균 연봉을 출력*/
SELECT 부서.부서명, AVG(의사.연봉) AS 평균연봉
FROM 의사
JOIN 부서 ON 의사.부서명 = 부서.부서명
GROUP BY 부서.부서명
HAVING AVG(의사.연봉) >= 70000000;

/*FROM 절대 3개의 릴레이션을 사용하는 질의, IN 연산자를 사용하면서 각 연산자를 이용할때 서브질의 사용*/
/* '이비인후과' 또는 '정형외과'에 속한 의사들이 진료한 환자들의 이름을 조회 (WHERE 절에 서브쿼리 사용)*/
SELECT 환자.환자이름
FROM 환자
JOIN 진료 ON 환자.환자번호 = 진료.환자번호
JOIN 의사 ON 진료.의사번호 = 의사.의사번호
WHERE 환자.환자번호 IN (
    SELECT 진료.환자번호
    FROM 진료
    JOIN 의사 ON 진료.의사번호 = 의사.의사번호
    WHERE 의사.부서명 IN ('이비인후과', '정형외과')
);


/* fromw절에 3개의 릴레이션을 사용하며 where절에 some을 사용하고 서브 질의를 이용*/
/*'이비인후과' 의사들의 연봉이 어떠한 '정형외과' 의사들의 연봉보다 많은 환자들의 이름을 조회*/
SELECT 
    환자.환자이름,
    진료.진료코드,
    의사.의사이름
FROM 환자
JOIN 진료 ON 환자.환자번호 = 진료.환자번호
JOIN 의사 ON 진료.의사번호 = 의사.의사번호
WHERE 의사.부서명 = '이비인후과'
AND 의사.연봉 > SOME (
    SELECT 의사.연봉
    FROM 의사
    WHERE 의사.부서명 = '정형외과'
);


/*from 절에 3개의 릴레이션을 사용하며 where절에 exists를 사용하며 서브질의 이용*/
/*이비인후과 부서에 속한 의사가 진료한 환자들의 이름을 조회*/
SELECT 환자.환자이름
FROM 환자
JOIN 진료 ON 환자.환자번호 = 진료.환자번호
JOIN 의사 ON 진료.의사번호 = 의사.의사번호
WHERE EXISTS (
    SELECT 1
    FROM 진료 AS 서브진료
    JOIN 의사 AS 서브의사 ON 서브진료.의사번호 = 서브의사.의사번호
    WHERE 서브진료.환자번호 = 환자.환자번호
    AND 서브의사.부서명 = '이비인후과'
);


/*from 절에 3개의 릴레이션을 사용하여 where절에 unique를 사용하며 서브질의 이용*/
/*mysql에서는 unique를 지원하지 않으므로 DiSTINCT 와 GROUP BY**와 **HAVING**를 사용하여 서브쿼리에서 중복 없이 유일한 결과를 반환하는 방식으로 UNIQUE 연산자와 같은 효과를 낸다.*/
/*이비인후과 부서의 의사가 진료한 환자들 중에서, 중복 없이 유일한 환자번호만을 찾아서 그 환자들의 이름을 반환한다*/
SELECT DISTINCT 환자.환자이름
FROM 환자
JOIN 진료 ON 환자.환자번호 = 진료.환자번호
JOIN 의사 ON 진료.의사번호 = 의사.의사번호
WHERE 환자.환자번호 IN (
    SELECT 진료.환자번호
    FROM 진료
    JOIN 의사 ON 진료.의사번호 = 의사.의사번호
    WHERE 의사.부서명 = '이비인후과'
    GROUP BY 진료.환자번호
    HAVING COUNT(DISTINCT 진료.의사번호) = 1
);

/*from 절에 5개의 릴레이션을 사용하며 intersect를 사용한다.*/
/*mysql에서는 intersect를 지원하지 않으므로 환자.환자이름을 SELECT DISTINCT로 선택하여 중복을 제거하고 그리고 AND를 사용하여환자 이름을 출력하게하여 같은 효능을 내게 하였다.*/
/*이비인후과에서 진료를 받은 환자 중에서, 정형외과에서도 진료를 받은 환자들의 부서명, 의사 이름, 환자 이름, 진료 코드, 질병 코드를 출력*/
SELECT DISTINCT 
    부서.부서명,
    의사.의사이름,
    환자.환자이름,
    진료.질병코드,
    질병.질병명
FROM 부서
JOIN 의사 ON 부서.부서명 = 의사.부서명
JOIN 진료 ON 의사.의사번호 = 진료.의사번호
JOIN 환자 ON 진료.환자번호 = 환자.환자번호
JOIN 질병 ON 진료.질병코드 = 질병.질병코드
WHERE 의사.의사이름 = '이주은'
AND 환자.환자번호 IN (
    SELECT 환자.환자번호
    FROM 진료
    JOIN 의사 ON 진료.의사번호 = 의사.의사번호
    WHERE 의사.의사이름 = '이주은'
    GROUP BY 환자.환자번호
    HAVING COUNT(진료.진료코드) > 1
)
INTERSECT
SELECT DISTINCT
    부서.부서명,
    의사.의사이름,
    환자.환자이름,
    진료.질병코드,
    질병.질병명
FROM 부서
JOIN 의사 ON 부서.부서명 = 의사.부서명
JOIN 진료 ON 의사.의사번호 = 진료.의사번호
JOIN 환자 ON 진료.환자번호 = 환자.환자번호
JOIN 질병 ON 진료.질병코드 = 질병.질병코드
WHERE 의사.의사이름 = '이주은'
AND 환자.환자번호 IN (
    SELECT 환자.환자번호
    FROM 진료
    JOIN 의사 ON 진료.의사번호 = 의사.의사번호
    WHERE 의사.의사이름 = '이주은'
    GROUP BY 환자.환자번호
    HAVING COUNT(진료.진료코드) > 1
);


/* from절에 5개의 테이블을 이용하고 except문을 사용하여라*/
/* mysql에서는 except을 제공하지 않으므로 LEFT JOIN과 IS NULL을 사용하여 차집합을 구현하였다.*/
/*이주은에게 진료받고 박동주에게는 진료받지 않은 환자들의 정보를 출력*/

SELECT DISTINCT 
    부서.부서명,
    의사.의사이름,
    환자.환자이름,
    진료.진료코드,
    진료.질병코드
FROM 부서
JOIN 의사 ON 부서.부서명 = 의사.부서명
JOIN 진료 ON 의사.의사번호 = 진료.의사번호
JOIN 환자 ON 진료.환자번호 = 환자.환자번호
JOIN 질병 ON 진료.질병코드 = 질병.질병코드
WHERE 의사.의사이름 = '이주은'

-- 차집합 구현을 위한 LEFT JOIN
AND 환자.환자번호 NOT IN (
    SELECT 진료.환자번호
    FROM 진료
    JOIN 의사 ON 진료.의사번호 = 의사.의사번호
    WHERE 의사.의사이름 = '박동주'
);

/*from절에 5개의 테이블을 사용하고 union을 사용한다*/
/*이주은 또는 박동주에게 진료를 받은 환자들의 부서명, 의사이름, 환자이름, 진료코드, 질병코드를 출력하는 질의를 작성*/
-- 이주은에게 진료받은 환자들의 정보
SELECT 
    부서.부서명,
    의사.의사이름,
    환자.환자이름,
    진료.진료코드,
    진료.질병코드
FROM 부서
JOIN 의사 ON 부서.부서명 = 의사.부서명
JOIN 진료 ON 의사.의사번호 = 진료.의사번호
JOIN 환자 ON 진료.환자번호 = 환자.환자번호
JOIN 질병 ON 진료.질병코드 = 질병.질병코드
WHERE 의사.의사이름 = '이주은'

UNION

-- 박동주에게 진료받은 환자들의 정보
SELECT 
    부서.부서명,
    의사.의사이름,
    환자.환자이름,
    진료.진료코드,
    진료.질병코드
FROM 부서
JOIN 의사 ON 부서.부서명 = 의사.부서명
JOIN 진료 ON 의사.의사번호 = 진료.의사번호
JOIN 환자 ON 진료.환자번호 = 환자.환자번호
JOIN 질병 ON 진료.질병코드 = 질병.질병코드
WHERE 의사.의사이름 = '박동주';

/*데이터 수정(updata)*/
/*이주은의 연봉을 10배상승*/
-- 안전 업데이트 모드 비활성화
SET SQL_SAFE_UPDATES = 0; /*이 부분을 하지 않으니까 업데이트가 되지 않았습니다. Error Code: 1175는 MySQL에서 안전 업데이트 모드 (safe update mode) 
가 활성화된 경우 발생하는 오류입니다. 이 부분은 구글링을 참조하였습니다.*/
UPDATE 의사
SET 의사.연봉 = 의사.연봉*10
WHERE 의사.의사이름 = '이주은';

SELECT * 
FROM 의사
WHERE 의사.의사이름='이주은';

/*데이터 삭제(delete)*/
/*진료코드 D7 삭제*/
DELETE FROM 진료
WHERE 진료코드 = 'D7';

SELECT * 
FROM 진료;


/*1개의 기본 릴레이션 (base relation)을 갖는 뷰를 1개 생성하고*/
CREATE VIEW 의사_정보 AS
SELECT 의사이름, 연봉
FROM 의사;

SELECT *
FROM 의사_정보
WHERE 의사이름='이주은';

UPDATE 의사_정보
SET 의사이름 = '삐끼삐끼이주은'
WHERE 의사이름 = '이주은';

SELECT *
FROM 의사_정보;

-- 연봉 변경 로그 테이블 생성 (예시)
CREATE TABLE 연봉변경로그 (
    의사이름 VARCHAR(10),
    이전연봉 INT,
    변경연봉 INT,
    변경일 TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 트리거 생성: 의사 테이블의 연봉이 변경될 때마다 연봉변경로그에 기록
DELIMITER $$
CREATE TRIGGER 연봉_변경_트리거
AFTER UPDATE ON 의사
FOR EACH ROW
BEGIN
    -- 연봉이 변경되었을 때만 로그를 기록
    IF OLD.연봉 <> NEW.연봉 THEN
        INSERT INTO 연봉변경로그 (의사이름, 이전연봉, 변경연봉)
        VALUES (NEW.의사이름, OLD.연봉, NEW.연봉);
    END IF;
END $$
DELIMITER ;

-- 예시: 이주은 의사의 연봉을 10% 상승
UPDATE 의사
SET 연봉 = 연봉 * 1.1
WHERE 의사이름 = '삐끼삐끼이주은';

-- 연봉변경로그 테이블 확인
SELECT * FROM 연봉변경로그;







