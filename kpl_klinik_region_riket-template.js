
			var colors = ["#00B3F6","#FFB117",'#F8F8F8',"#80B286"];
			var colors_tint = ['#80A3C3','#CCCCCC'];

			var defaultChart1 = {
				chartContent: null,
				highchart: null,
				defaults: {
					credits: {
						enabled: false
					},
					chart: {
						type: 'bar',
						plotBackgroundColor: colors[2],
						margin: [0,20,12,0],
						plotBorderWidth: 0,
						plotBorderColor: colors[2]
					},
					plotOptions: {
						bar: {
							pointWidth: 15
						}
					},
					title: {
						text: ''
					},
					xAxis: {
						categories: ['',''],
						labels: {
							style: {
								fontSize: '12px',
								fontFamily: 'verdana',
								width: '35px',
								color: '#000000'
							}
						},
						lineColor: 'transparent',
						tickLength: 0
					},
					legend: {
						enabled: false
					}
				},
				init: function(options) {
					this.highchart= jQuery.extend({}, this.defaults, options);
					this.highchart.chart.renderTo = this.chartContent;
				},
				create: function() {
					return new Highcharts.Chart(this.highchart);
				}
			};

			var defaultChart2 = {
				chartContent: null,
				highchart: null,
				defaults: {
					credits: {
						enabled: false
					},
					chart: {
						type: 'column',
						plotBackgroundColor: colors[2],
						margin: [0,20,12,0],
						plotBorderWidth: 0,
						plotBorderColor: colors[2]
					},
					plotOptions: {
						column: {
							pointWidth: 25
						}
					},
					title: {
						text: ''
					},
					yAxis: {
						min: 0,
						max: 100,
						lineColor: 'transparent',
						tickLength: 0,
						title: {
							text: ''
						},
						labels: {
								enabled: false
							},
						gridLineWidth: 0
					},
					tooltip: {
						enabled: false
					},
					legend: {
						enabled: false
					}
				},
				init: function(options) {
					this.highchart= jQuery.extend({}, this.defaults, options);
					this.highchart.chart.renderTo = this.chartContent;
				},
				create: function() {
					return new Highcharts.Chart(this.highchart);
				}
			};

			function createChart1(containerName,y1,y2,y3,l1,l2,stackednames) {

				var series = null, tempTooltip = false;
				if (y1!=null && y1.length==2) {
					tempTooltip = true;
					series = [
						{name: stackednames[1], data: [{y:y1[1],color:colors_tint[0],borderColor:colors_tint[0]},{y:y2[1],color:colors_tint[1],borderColor:colors_tint[1]}]},
						{name: stackednames[0], data: [{y:y1[0],color:colors[0],borderColor:colors[0]},{y:y2[0],color:colors[1],borderColor:colors[1]},]}
					];
				} else {
					series = [
						{data: [{y:y1,color:colors[0],borderColor:colors[0]}, {y:y2,color:colors[1],borderColor:colors[1]}, {y:y3,color:colors[3],borderColor:colors[0]}]}
					];
				}

				var tempTickPos = [0,l1,l2,100];

				var tempChart = {
					chartContent: containerName,
					options: {
						yAxis: {
							labels: {
								formatter: function() {
									if (this.value===l1 || this.value===l2) {
										return this.value;
									} else {
										return '';
									}
								},
								style: {
									fontSize: '8px',
									fontFamily: 'verdana',
									color: '#606060'
								},
								y: 12
							},
							tickPositions: tempTickPos,
							gridLineWidth: 0,
							plotLines:	[{
											color: '#606060',
											width: 1,
											value: l1,
											zIndex: 5
										},{
											color: '#606060',
											width: 1,
											value: l2,
											zIndex: 5
										}]
						},
						plotOptions: {
							bar: {
								pointWidth: 15
							},
							series: {
								stacking: 'normal'
							}
						},
						tooltip: {
							enabled: tempTooltip,
							formatter: function() {
								return '<b>' + this.series.name + '</b>';
							}
						},
						series: series
					}
				};
				tempChart = jQuery.extend(true, {}, defaultChart1, tempChart);
				tempChart.init(tempChart.options);
				var chartFinal = tempChart.create();

				chartFinal.renderer.text('%',130,80).css({
					fontSize: '8px',
					fontFamily: 'verdana',
					color: '#606060',
					textAlign: 'left'
				}).attr({
					zIndex: 999
				}).add();

				return chartFinal;
			}

			function createChart2(containerName,y) {

				var ytxt = y.slice(0);
				for (i = 0; i < ytxt.length; i++) {
					if (ytxt[i] == null) {ytxt[i] = '-';}
				}

				var tempChart = {
					chartContent: containerName,
					options: {
						xAxis: {
							categories: ytxt,
							tickLength: 0,
							labels: {
								style: {
									fontSize: '8px',
									fontFamily: 'verdana',
									color: '#606060'
								},
								y: 12
							}
						},
						series: [
							{data: [{y:y[0],color:colors[0],borderColor:colors[0]}, {y:y[1],color:colors[0],borderColor:colors[0]}, {y:y[2],color:colors[0],borderColor:colors[0]}, {y:y[3],color:colors[0],borderColor:colors[0]}]}
						]

					}
				};
				tempChart = jQuery.extend(true, {}, defaultChart2, tempChart);
				tempChart.init(tempChart.options);
				var chartFinal = tempChart.create();

				chartFinal.renderer.text('%',130,80).css({
					fontSize: '8px',
					fontFamily: 'verdana',
					color: '#606060',
					textAlign: 'left'
				}).attr({
					zIndex: 999
				}).add();

				return chartFinal;
			}

			function createIndRow(counter,shorttitle,longtitle,num1,den1,num2,den2,num3,den3,placnum,placden,history,l1,l2,group,description,stackednames,groupdividers) {

				var stacked = false;
				if (num1.length==2) { stacked = true; }

				if (stacked==true) {
					var pct1 = [null,null], pct2 = [null,null], sign = null, sumnum1 = null, sumpct1 = null, sumpct2 = null, pct3 = [null,null], sumpct3 = null;
					if (den1 > 0) {
						for (i = 0; i < 2; i++) {
							pct1[i] = 100*num1[i]/den1;
							sumnum1 = sumnum1 + num1[i];
							sumpct1 = sumpct1 + pct1[i];
						}
						sign = 3;
						if (sumpct1 >= l1) {sign = 2;}
						if (sumpct1 >= l2) {sign = 1;}
						sumpct1 = Math.round(sumpct1);
					}
					if (den2 > 0) {
						for (i = 0; i < 2; i++) {
							pct2[i] = 100*num2[i]/den2;
							sumpct2 = sumpct2 + pct2[i];
						}
						sumpct2 = Math.round(sumpct2);
					}
					if (den3 > 0) {
						for (i = 0; i < 2; i++) {
							pct3[i] = 100*num3[i]/den3;
							sumpct3 = sumpct3 + pct3[i];
						}
						sumpct3 = Math.round(sumpct3);
					}
					for (i = 0; i < history.length; i++) {
						if (history[i] != null) {
							history[i] = Math.round(history[i]);
						}
					}
				} else {
					var pct1 = null, pct2 = null, sign = null, sumnum1 = null, sumpct1 = null, sumpct2 = null, pct3=null, sumpct3 = null;
					if (den1 > 0) {
						pct1 = 100*num1/den1;
						sign = 3;
						if (pct1 >= l1) {sign = 2;}
						if (pct1 >= l2) {sign = 1;}
						sumnum1 = num1;
						sumpct1 = Math.round(pct1);
					}
					if (den2 > 0) {
						pct2 = 100*num2/den2;
						sumpct2 = Math.round(100*num2/den2);
					}
					if (den3 > 0) {
						pct3 = 100*num3/den3;
						sumpct3 = Math.round(100*num3/den3);
					}
					for (i = 0; i < history.length; i++) {
						if (history[i] != null) {
							history[i] = Math.round(history[i]);
						}
					}
				}

				var returnpoint = 0;

				var table = document.getElementById('bodyTable'),
					tr = document.createElement('tr'),
					td,
					content
				;

				if ($.inArray(counter,groupdividers) > -1) {
					$(tr).attr('class','divider');
				}

				td = document.createElement('td');
				$(td).attr('class','margin');
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indsign');
					if (sign != null) {

						returnpoint = 3 - sign;

						content = document.createElement('img');
						$(content).attr('width','35px');
						$(content).attr('src','{{url|.}}/Public/Files/Prostataregistret/npcr_kpl/img/npcr_act'+sign+'.png');
						td.appendChild(content);
					}
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indtext');
					content = document.createElement('div');
					$(content).attr('class','shorttitle');
					$(content).html(counter + '. ' + shorttitle);
					td.appendChild(content);
					content = document.createElement('div');
					$(content).attr('class','longtitle');
					$(content).html(longtitle);
					td.appendChild(content);
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indpat');

					content = document.createElement('div');
					$(content).attr('style','padding:5px 0px;');
					if (den1 > 0) {
						$(content).html(sumnum1 + ' av ' + den1);
					} else {
						$(content).html('Ej redovisad');
					}
					td.appendChild(content);
					content = document.createElement('div');
					$(content).attr('style','padding:5px 0px;');
					if (den2 > 0) {
						$(content).html(num2 + ' av ' + den2);
					} else {
						$(content).html('Ej redovisad');
					}
					td.appendChild(content);

					content = document.createElement('div');
					$(content).attr('style','padding:5px 0px;');
					if (den2 > 0) {
						$(content).html(num3 + ' av ' + den3);
					} else {
						$(content).html('Ej redovisad');
					}
					td.appendChild(content);
					content = document.createElement('div');
					$(content).html('&nbsp;');
					td.appendChild(content);
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indpct');
					content = document.createElement('div');
					$(content).attr('style','padding:5px 0px;');
					if (sumpct1 != null) {
						$(content).html(sumpct1 + ' %');
					} else {
						$(content).html('- %');
					}
					td.appendChild(content);
					content = document.createElement('div');
					$(content).attr('style','padding:5px 0px;');
					if (sumpct2 != null) {
						$(content).html(sumpct2 + ' %');
					} else {
						$(content).html('- %');
					}
					td.appendChild(content);
					content = document.createElement('div');
					$(content).attr('style','padding:5px 0px;');
					if (sumpct2 != null) {
						$(content).html(sumpct3 + ' %');
					} else {
						$(content).html('- %');
					}
					td.appendChild(content);
					content = document.createElement('div');
					$(content).html('&nbsp;');
					td.appendChild(content);
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','fig');
					content = document.createElement('div');
					$(content).attr('id','fig1ind'+counter);
					$(content).attr('style','width:140px;height:80px;');
					td.appendChild(content);
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','fig');
					content = document.createElement('div');
					$(content).attr('id','fig2ind'+counter);
					$(content).attr('style','width:140px;height:80px;');
					td.appendChild(content);
				tr.appendChild(td);

				//td = document.createElement('td');
				//$(td).attr('class','indplac');
				//	if (placnum != null && placden != null) {
				//		content = document.createElement('span');
				//		$(content).attr('class','plac1');
				//		$(content).html(placnum);
				//		td.appendChild(content);
				//		content = document.createElement('span');
				//		$(content).attr('class','plac2');
				//		$(content).html('/' + placden);
				//		td.appendChild(content);
				//	}
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','margin');
				tr.appendChild(td);

				table.appendChild(tr);

				createChart1('fig1ind'+counter,pct1,pct2,pct3,l1,l2,stackednames);
				createChart2('fig2ind'+counter,history);

				var descrGroup = document.getElementById('descrGroup' + group);
				content = document.createElement('div');
				$(content).attr('class','indexDescription');
				$(content).html(counter + '. ' + description);
				descrGroup.appendChild(content);

				return returnpoint;

			}

			function AddSumRow(sumToPrint,txtToPrint) {

				var table = document.getElementById('bodyTable'),
					tr = document.createElement('tr'),
					td,
					content
				;

				td = document.createElement('td');
				$(td).attr('class','margin');
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indsumnum');
				$(td).attr('id','indsumnum');
				$(td).html(sumToPrint);
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indsumtxt');
				$(td).html(txtToPrint);
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indsumfill');
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indsumfill');
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indsumfill');
				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','indsumfill');
				tr.appendChild(td);

				//td = document.createElement('td');
				//$(td).attr('class','indsumfill');
				//tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','margin');
				tr.appendChild(td);

				table.appendChild(tr);

			}

			function CreateDescrTable(descrGroups) {

				var table = document.getElementById('descrTable'),
					tr = document.createElement('tr'),
					td,
					content
				;

				td = document.createElement('td');
				$(td).attr('class','margin');
				tr.appendChild(td);

				td = document.createElement('td');

					content = document.createElement('div');
					$(content).attr('id','indexDescriptionTitle');
					$(content).attr('class','indexDescriptionTitle');
					$(content).html('Definition av indikatorer');
					td.appendChild(content);

					for (i = 0; i < descrGroups.length; i++) {
						//content = document.createElement('div');
						//$(content).attr('id','indexDescriptionSubTitle' + (i+1));
						//$(content).attr('class','indexDescriptionSubTitle');
						//$(content).html(descrGroups[i]);
						td.appendChild(content);

						content = document.createElement('div');
						$(content).attr('id','descrGroup' + (i+1));
						$(content).attr('class','indexDescription');
						td.appendChild(content);
					}

				tr.appendChild(td);

				td = document.createElement('td');
				$(td).attr('class','margin');
				tr.appendChild(td);

				table.appendChild(tr);
			}

			$(function() {

				//$('a.infolink').hover(function () {
				//	$('> div', this).fadeIn();
				//},
				//function () {
				//	$('> div', this).fadeOut();
				//});

				$(document).on('click', 'a.printLink', function() {
					//window.print();
					document.execCommand( 'print', false, null ) ;
				});

			});
