var charts = [];
var chartIndices = [];
var chartIds = [];
var futureCharts = [];
var resultStore = [];
var displayedChart = null;
var firstChart = true;
var chartsLoaded = false;
var futureChartsLoaded = false;
var userIdValid = false;
var userId = null;
var position = 1;

function initialize() {

    enableNextButton(false);
    $("#nextButton").click(nextChart);

    var userId = $("#userId");
    userId.keydown(filterNonNumbers);
    userId.keyup(validateUser);
    userId.focus();

    $(".ratings").hide();
    $("input[name='direction']").click(radioClicked);
    $("input[name='conviction']").click(radioClicked);

    d3.csv("CHART_SERIES.csv", loadCharts);
}

function filterNonNumbers(e) {
    // Courtesy of http://stackoverflow.com/questions/995183/how-to-allow-only-numeric-0-9-in-html-inputbox-using-jquery
    // Allow: backspace, delete, tab, escape, enter and .
    if ($.inArray(e.keyCode, [46, 8, 9, 27, 13, 110, 190]) !== -1 ||
        // Allow: Ctrl+A
        (e.keyCode == 65 && e.ctrlKey === true) ||
        // Allow: home, end, left, right
        (e.keyCode >= 35 && e.keyCode <= 39)) {
        // let it happen, don't do anything
        return;
    }
    // Ensure that it is a number and stop the keypress
    if ((e.shiftKey || (e.keyCode < 48 || e.keyCode > 57)) && (e.keyCode < 96 || e.keyCode > 105)) {
        e.preventDefault();
    }
}

function enableNextButton(enabled) {
    $("#nextButton").prop("disabled", !enabled);
}

function validateUser(source) {
    var fiveDigits = source.target.value.length == 5;
    userIdValid = fiveDigits && parseInt(source.target.value) % 7 == 0;
    if (userIdValid) {
        userId = source.target.value;
        $("#invalidId").hide();

    } else if (fiveDigits) {
        $("#invalidId").show();
    }
    enableStart();
}

function enableStart() {
    var enabled = chartsLoaded && userIdValid;
    enableNextButton(enabled);
    if (enabled) $("#nextButton").focus();
}

function isUndefined(v) {
    return typeof v === "undefined";
}

function radioClicked(source) {
    // Set currentChart direction/conviction equal to the radio button's enclosing label
    displayedChart[source.target.name] = source.target.parentElement.textContent.trim();

    // Enable nextButton if both radio buttons have been clicked
    var disabled = isUndefined(displayedChart.direction) || isUndefined(displayedChart.conviction);
    $("#nextButton").prop("disabled", disabled);
    if (!disabled) $("#nextButton").focus();
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
    chartIndices = _.first(chartIndices, 5);

    chartIndices.reverse(); // so that we can use pop()
    chartIds = _.map(chartIndices, function(i){return charts[i]['id']})
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
    // $("#main_content").text("Done");

    // Display results
    revealFuture();
}

function drawChart(data) {
    var margin = {top: 20, right: 20, bottom: 30, left: 50},
        width = 800 - margin.left - margin.right,
        height = 400 - margin.top - margin.bottom;


    var x = d3.time.scale()
        .range([0, width]);

    var y = d3.scale.linear()
        .range([height, 0]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom")
        .tickFormat(d3.time.format("%b"));

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

    resultStore.push(result);

    $.post("results", result)
}


function revealFuture(){

    // load future data & charts
    d3.csv("webfiles/CHART_RESULT_SERIES.csv", loadFutureCharts);
}

function loadFutureCharts(data) {
    var parseDate = d3.time.format("%Y-%m-%d").parse;

    var currentChart = {id: -1};

    var subset = _.filter(data, function(obj){return _.contains(chartIds, obj.groupid)});

    subset.forEach(function (row) {
        if (row.groupid !== currentChart.id) {
            currentChart = {
                id: row.groupid,
                group: row.group,
                underlyer: row.underlyer,
                conviction: _.findWhere(resultStore, {chart:row.groupid }).conviction,
                direction: _.findWhere(resultStore, {chart:row.groupid }).direction,
                data: []
            };
            futureCharts.push(currentChart);
        }
        currentChart.data.push({
            date: parseDate(row.date),
            close: parseFloat(row.value),
            type: row.type
        });
    });

    futureChartsLoaded = true;
    for(var i=0;i<futureCharts.length;i++){
        drawFutureCharts(futureCharts[i]);
    }
    
}


function drawFutureCharts(data) {


    var margin = {top: 50, right: 20, bottom: 30, left: 50},
        width = 800 - margin.left - margin.right,
        height = 400 - margin.top - margin.bottom;


    var x = d3.time.scale()
        .range([0, width]);

    var y = d3.scale.linear()
        .range([height, 0]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom")
        .tickFormat(d3.time.format("%b"));

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

    x.domain(d3.extent(data.data, function (d) { return d.date; }));
    y.domain(d3.extent(data.data, function (d) { return d.close; }));

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
        .datum(_.filter(data.data, function(obj){return obj.type==='past'}))
        .attr("class", "line past")
        .attr("d", line);

    svg.append("path")
        .datum(_.filter(data.data, function(obj){return obj.type==='future'}))
        .attr("class", "line future")
        .attr("d", line);

    svg.append('text')
        .datum(data)
        .attr({"x":"30", "y":"-5", "fill":"black"})
        .text(function(d){return d.underlyer + ": " + d.direction + " " + d.conviction})
}