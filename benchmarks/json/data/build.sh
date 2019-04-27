#!/bin/bash

# a few tweet ids
tweets='1060215684033114114 1050047249567416326 1019252521498501120 1016382069172326401 1010149382946197504 1009915721374150657 1009914672651427840 1009913257694556166 1009808171429449729 1009787926706900993 1009420018654859269 1009418896204615682 1008507220764577792 1006616961999089664 1002194915835744256 996046728783523841 991114495245324288 981933571291021313 979475722191466496 979370488337223680 979369086756016128 974310832237694977 974043991162392577 972192134660059143 966854613654786049 965375971833430016 965073992938934272 951512281312935938 950529005169487872 938132725801914368 936710024675635200 931697091465924613 931310927219060736 926105120927920128 917531717111615494 904817786001776640 902674868897857536 904048312638205952 879488996707098624 877976514297380864 877856421949329408 877437885347135488 877190891039019009 877062558456401920 876835255566643200 876376328605401088 876068990602407937 870634438140493824 867953403787304961 857975217163038720 857493227242479616 854420118281105408 854149212405207045 849302317216272387 846475507831058432 841615971001520130 840713567955484672 833722900330975237 833705083686752256 824780041175367680 823583161053376512 816343819465752578 801173741409439748 798902332830482432 798673101886099457 798590449560784896 796144462451867649 793594057071230976 781741860175872000 779823263480512512 779821815057317889 743881821193019393 743916473584750592 743915917394878465 743914166667214848 743868957015216128 743858688918593537 743849285003804672 743852058697031681 743833102909202432 743705538681155584 743818288338112512 743833390537838593 743833994190426112 743832698402144257 743713625005654016 743592638121598976 743696911907971072 743663982888660992 743606774368972800 743597003674771456 743590764085575680 743587489407860736 743580553434148864 743582760036507648 743567484049993728 743560459777773569 743550076241358849 743554125166612480 743524924556357632 743504307631202305 743498151265394688 743487377209860096 743482276311228419 743479424289406976 743479021028073472 743478540826357760 743478439651377153 743474595391410176 743473085873946625 743454007339814912 743217789905711104 743195043188477953 743153863297884160 743140200755302401 743125607710523392 743125738069491712 743124335376818177 743120112970956800 743119082581430274 743114378493337601 743112702852141058 743110273318326272 743109231100854274 743109466514591745 743108093727232001 743084998362681344 742859988398280709 742858275784253441 742780717873012736 742754844688023552 742753156027711488 742744532844875777 742751144905080832 742736824821391362 742736737508544512 742491535929794560 740979352834215936 740563099460227072 739103680574676992 738057002929270784 738055144735002625 735515881686458368 734395464053870595 718532159980441600 715010313972625408 714935145346629632 714935000651534336 710882487702925312 709384125334089728 705468692641878016 705066189396037633 705050007548747776 704361508579311616 690662268263124993 673918957867241472 667744313829253121 666986429453570048 662332262583660544 652506844959768576 652187175849730048 643925629226758145 611310291243167744 611275980863221760 611222445429690368 611221217924681728 611216498061774850 610937861085343744 610948619324166144 610931585404268544 610881265055698944 610878673323958272 610843570166104065 610843045160816642 610578171377909760 610565708095754241 610556629415636992 610546457666392064 610545993495347201 610545051098157056'
rm all.json
for id in ${tweets}; do
    GET https://publish.twitter.com/oembed?url=https%3A%2F%2Ftwitter.com%2FInterior%2Fstatus%2F${id} > ${id}.json
done
cat *.json > tmpfile && mv tmpfile all.json