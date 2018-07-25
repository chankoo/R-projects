
### R 시각화 발제

Ref:

- R을 이용한 데이터 처리 & 분석 실무[1]
https://thebook.io/006723/ch06/

- R로 하는 데이터 시각화: Ch4 ggplot2를 이용 데이터 시각화 [2]

- 보고용 차트를 위한 ggplot2 사용하기[3]
https://mrchypark.github.io/data_camp_dabrp/ggplot.html

- 공식문서[4] http://docs.ggplot2.org




#### R 기본 그래프

ggplot이 워낙 강력하므로 간단한 기능 위주로 다뤄보는 것을 추천합니다. ref[1]에 정리가 잘 되어있습니다.

산점도 : https://thebook.io/006723/ch06/01/

그래프 옵션: https://thebook.io/006723/ch06/02/

기본 그래프들 :https://thebook.io/006723/ch06/03/

legend: https://thebook.io/006723/ch06/06/

boxplot:https://thebook.io/006723/ch06/08/01/

histogram: https://thebook.io/006723/ch06/08/02/


#### ggplot2

"ggplot2는 데이터를 이해하는 데 더 좋은 시각화 툴"

ggplot 객체를 만들어 레이어를 계속 쌓는 특유의 문법에 익숙해질 필요가 있습니다. 

함수 내부 규정에 종속되어 데이터 입력 포맷에 제한을 받는 기본 그래픽스보다 훨씬 더 편하고 효과적인 기능을 제공합니다.

__확실히 개념 잡기__ ref[2]

0. geom 객체

1. 미적 매핑(aes)

2. 통계적 변환(stat)

3. 위치 조정



__예제 __ref[3]

