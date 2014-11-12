var charts = [];
var chartIndices = [];
var displayedChart = null;
var firstChart = true;
var chartsLoaded = false;
var userIdValid = false;
var userId = null;
var position = 1;

function initialize() {

    enableNextButton(false);
    $("#nextButton").click(nextChart);

    $("#userId").keyup(validateUser);

    $(".ratings").hide();
    $("input[name='direction']").click(radioClicked);
    $("input[name='conviction']").click(radioClicked);

    d3.csv("CHART_SERIES.csv", loadCharts);
}

function enableNextButton(enabled) {
    $("#nextButton").prop("disabled", !enabled);
}

function validateUser(source) {
    userIdValid = source.target.value.length == 5 && parseInt(source.target.value) % 7 == 0;
    if (userIdValid) userId = source.target.value;
    enableStart();
}

function enableStart() {
    enableNextButton(chartsLoaded && userIdValid);
}

function isUndefined(v) {
    return typeof v === "undefined";
}

function radioClicked(source) {
    // Set currentChart direction/conviction equal to the radio button's enclosing label
    displayedChart[source.target.name] = source.target.parentElement.innerText;

    // Enable nextButton if both radio buttons have been clicked
    $("#nextButton").prop("disabled", isUndefined(displayedChart.direction) || isUndefined(displayedChart.conviction));
}

function loadCharts(data) {
    var parseDate = d3.time.format("%Y-%m-%d").parse;

    var currentChart = {id: -1};

    data.forEach(function (row) {
        if (row.groupid !== currentChart.id) {
            currentChart = {
                id: row.groupid,
                group: row.group,
                data: []
            };
            charts.push(currentChart);
        }
        currentChart.data.push({
            // TODO: did we want "anonymous" dates?
            date: parseDate(row.date),
            close: parseFloat(row.value)
        });
    });

    randomize();

    chartsLoaded = true;
    enableStart();
}

function randomize() {
    chartIndices = _.range(charts.length);

    // TODO: block randomization
    chartIndices = _.shuffle(chartIndices);

    // TODO: remove in production
    chartIndices = _.first(chartIndices, 2);

    chartIndices.reverse(); // so that we can use pop()
}

function nextChart() {
    $("#main_content").empty();
    $("#nextButton").prop("disabled", true);

    if (firstChart) {
        $("#nextButton").text("Next Chart");
        $(".ratings").show();
        $("#userId").prop("disabled", true);
        firstChart = false;

    } else {
        postResults();
        position += 1;
    }

    $("input[name='direction']").removeAttr("checked");
    $("input[name='conviction']").removeAttr("checked");

    if (chartIndices.length > 0) {
        displayedChart = charts[chartIndices.pop()];
        $("#chartId").text(displayedChart.id);
        drawChart(displayedChart.data);

    } else {
        done();
    }

}

function done() {
    $("#controls").hide();
    // TODO
    $("#main_content").text("Done");
}

function drawChart(data) {
    var margin = {top: 20, right: 20, bottom: 30, left: 50},
        width = 600 - margin.left - margin.right,
        height = 300 - margin.top - margin.bottom;


    var x = d3.time.scale()
        .range([0, width]);

    var y = d3.scale.linear()
        .range([height, 0]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom");

    var yAxis = d3.svg.axis()
        .scale(y)
        .orient("left");

    var line = d3.svg.line()
        .x(function (d) { return x(d.date); })
        .y(function (d) { return y(d.close); });

    //var svg = d3.select("body").append("svg")
    var svg = d3.select("#main_content").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    x.domain(d3.extent(data, function (d) { return d.date; }));
    y.domain(d3.extent(data, function (d) { return d.close; }));

    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);

    svg.append("g")
        .attr("class", "y axis")
        .call(yAxis)
        .append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("dy", ".71em")
        .style("text-anchor", "end")
        .text("Price ($)");

    svg.append("path")
        .datum(data)
        .attr("class", "line")
        .attr("d", line);
}

function postResults() {
    var result = {
        time: Date.now(),
        position: position,
        user: userId,
        chart: displayedChart.id,
        group: displayedChart.group,
        direction: displayedChart.direction,
        conviction: displayedChart.conviction
    }

    $.post("results", result)
}