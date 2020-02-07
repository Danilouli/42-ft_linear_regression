var data = [30, 86, 168, 281, 303, 365];

d3.select(".chart")
  .selectAll("div")
  .data(data)
    .enter()
    .append("div")
    .style("width", function(d) { return d + "px"; })
    .text(function(d) { return d; });




// var ctx = document.getElementById("myChart");
// var data = {
// 	labels: Array.from(Array(18).keys()).map(i=>10000*(i+6)),
//     datasets: [
// 	{
//         label: "regression",
//         function: function(x) { return (-0.017441*x + 6442) },
//         borderColor: "rgba(75, 192, 192, 1)",
//         data: [],
//         fill: false
// 	},
// 	// {
// 	// 	type : 'scatter',
// 	// 	label: 'scatter',
// 	// 	data: [
// 	// 		{x :240000, y :3650},
// 	// 		{x :139800, y : 3800},
// 	// 		{x :150500, y : 4400},
// 	// 		{x :185530, y : 4450},
// 	// 		{x :176000, y : 5250},
// 	// 		{x :114800, y : 5350},
// 	// 		{x :166800, y : 5800},
// 	// 		{x :89000, y : 5990},
// 	// 		{x :144500, y : 5999},
// 	// 		{x :84000, y : 6200},
// 	// 		{x :82029, y : 6390},
// 	// 		{x :63060, y : 6390},
// 	// 		{x :74000, y : 6600},
// 	// 		{x :97500, y : 6800},
// 	// 		{x :67000, y : 6800},
// 	// 		{x :76025, y : 6900},
// 	// 		{x :48235, y : 6900},
// 	// 		{x :93000, y : 6990},
// 	// 		{x :60949, y : 7490},
// 	// 		{x :65674, y : 7555},
// 	// 		{x :54000, y : 7990},
// 	// 		{x :68500, y : 7990},
// 	// 		{x :22899, y : 7990},
// 	// 		{x :61789, y : 8290}
// 	// 	]
// 	// },
// 	{
// 		label: 'point',
// 		backgroundColor: 'rgba(0,0,0,0.7)',
// 		borderColor: '#fff',
// 		borderWidth: 1,
// 		radius: 7,
// 		type: 'bubble',
// 		data: [
// 			{
// 				y: 4400,
// 				x: 150500
// 			}
// 		]
// 	}
// 	]
// };

// Chart.pluginService.register({
//     beforeInit: function(chart) {
//         var data = chart.config.data;
//         for (var i = 0; i < data.datasets.length; i++) {
//             for (var j = 0; j < data.labels.length; j++) {
// 				let label = data.datasets[i].label;
// 				if (label == 'point')
// 					continue;
//             	var fct = data.datasets[i].function,
//                 	x = data.labels[j],
//                 	y = fct(x);
//                 data.datasets[i].data.push(y);
//             }
//         }
//     }
// });

// var myBarChart = new Chart(ctx, {
//     type: 'line',
//     data: data,
//     options: {
//         scales: {
//             yAxes: [{
//                 ticks: {
//                     beginAtZero:true
//                 }
//             }]
//         }
//     }
// });