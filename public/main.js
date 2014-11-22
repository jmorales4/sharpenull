var charts = [];
var chartIndices = [];
var futureChartIndices = [];
var chartIds = [];
var futureCharts = [];
var resultStore = [];
var userIds = [];
var displayedChart = null;
var firstChart = true;
var chartsLoaded = false;
var futureChartsLoaded = false;
var userIdsLoaded = false;
var userIdValid = false;
var needSurvey = false;
var surveyResults = null;
var userId = null;
var position = 1;
var blocks = ['VNEG', 'NEG', 'FLAT', 'POS', 'VPOS'];
var chartsPerBlock = 1; // TODO: should be 2 in production?

function initialize() {

    enableNextButton(false);
    $("#nextButton").click(nextChart);

    var userId = $("#userId");
    userId.keydown(filterNonNumbers);
    userId.keyup(validateUser);
    userId.focus();

    $(".surveyRadio").click(checkSurvey);
    $(".surveyText").keydown(filterNonNumbers);
    $(".surveyText").keyup(checkSurvey);
    ;

    $(".ratings").hide();
    $("input[name='direction']").click(radioClicked);
    $("input[name='conviction']").click(radioClicked);

    d3.csv("CHART_SERIES.csv", loadCharts);
    d3.csv("CHART_RESULT_SERIES.csv", loadFutureCharts);
    d3.csv("8214ba9ab15f49/users.dat", loadUserIds);
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
    var enabled = userIdsLoaded && futureChartsLoaded && chartsLoaded && userIdValid;
    if (enabled) blockRandomize();
    needSurvey = userIdValid && userIds.indexOf(userId) < 0;
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
            date: parseDate(row.date),
            close: parseFloat(row.value)
        });
    });

    chartsLoaded = true;
    enableStart();
}

function blockRandomize() {
    var showCharts = []
    for (var i = 0; i < blocks.length; i++) {
        var bcharts = _.filter(charts, function (chart) {return chart.group === blocks[i]});
        showCharts = showCharts.concat(_.sample(bcharts, chartsPerBlock));
    }
    showCharts = _.shuffle(showCharts);

    var dig1 = userId[0];
    var block1;
    if (dig1 === "0" | dig1 === "1") {
        block1 = "VNEG";
    } else if (dig1 === "2" | dig1 === "3") {
        block1 = "NEG";
    } else if (dig1 === "4" | dig1 === "5") {
        block1 = "FLAT";
    } else if (dig1 === "6" | dig1 === "7") {
        block1 = "POS";
    } else {
        block1 = "VPOS";
    }

    var chartBlocks = _.pluck(showCharts, 'group');
    var moveIndex = _.indexOf(chartBlocks, block1);

    var toMove = showCharts.splice(chartBlocks.indexOf(block1), 1);
    showCharts.push(toMove[0]);

    chartIndices = [];
    chartIds = [];
    _.each(showCharts, function (charti) {
        chartIndices.push(_.indexOf(_.pluck(charts, 'id'), charti.id));
        futureChartIndices.push(_.indexOf(_.pluck(futureCharts, 'id'), charti.id));
    })
    futureChartIndices.reverse();
}

function nextChart() {
    $("#main_content").empty();
    $("#nextButton").prop("disabled", true);

    if (needSurvey) {
        $("#nextButton").text("Done");
        $("#userId").prop("disabled", true);
        return showSurvey();
    }

    if (firstChart) {
        $("#survey").hide();
        if (surveyResults !== null)
            $.post("survey", surveyResults)

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
        time: new Date().toISOString(),
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

function revealFuture() {
    for (var i = 0; i < futureChartIndices.length; i++) {
        displayedChart = futureCharts[futureChartIndices[i]];
        displayedChart.conviction = _.findWhere(resultStore, {chart: displayedChart.id}).conviction;
        displayedChart.direction = _.findWhere(resultStore, {chart: displayedChart.id}).direction;
        drawFutureCharts(displayedChart);
    }
}

function loadFutureCharts(data) {
    var parseDate = d3.time.format("%Y-%m-%d").parse;

    var currentChart = {id: -1};

    data.forEach(function (row) {
        if (row.groupid !== currentChart.id) {
            currentChart = {
                id: row.groupid,
                group: row.group,
                underlyer: row.underlyer,
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

}

function drawFutureCharts(chart) {


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

    x.domain(d3.extent(chart.data, function (d) { return d.date; }));
    y.domain(d3.extent(chart.data, function (d) { return d.close; }));

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
        .datum(_.filter(chart.data, function (obj) {return obj.type === 'past'}))
        .attr("class", "line past")
        .attr("d", line);

    svg.append("path")
        .datum(_.filter(chart.data, function (obj) {return obj.type === 'future'}))
        .attr("class", "line future")
        .attr("d", line);

    svg.append('text')
        .datum(chart)
        .attr({"x": "30", "y": "-5", "fill": "black"})
        .text(function (d) {return d.underlyer + ": " + d.direction + " " + d.conviction})
}

function loadUserIds(data) {
    data.forEach(function (row) {
        userIds.push(row.id);
    });
    userIdsLoaded = true;
}

function showSurvey() {
    $("#main_content").empty();
    $("#survey").show();

}

function checkSurvey() {
    var fields = [
        'literacy',
        'knowledge',
        'experience',
        'professional',
        'years',
        'role',
        'personal',
        '401k',
        'investType',
        'stocks',
        'bonds',
        'derivatives',
        'other',
        'return'];

    surveyResults = {id: userId};
    _.each(fields, function (field) {
        $("input[name='" + field + "']").each(function (i, input) {
            switch (input.type) {
                case 'radio':
                    if (input.checked)
                        surveyResults[field] = input.parentElement.textContent.trim();
                    break;
                case 'text':
                    if (input.value.length > 0)
                        surveyResults[field] = input.value;
                    else
                        delete surveyResults[field];
            }
        });
    });

    var done = _.chain(fields)
        .map(function (field) {
            switch (field) {
                case 'years':
                case 'role':
                    if (surveyResults.professional == 'No') delete surveyResults[field];
                    return surveyResults.professional == 'No' || surveyResults.hasOwnProperty(field);
                case '401k':
                case 'investType':
                case 'stocks':
                case 'bonds':
                case 'derivatives':
                case 'other':
                case 'return':
                    if (surveyResults.personal == 'No') delete surveyResults[field];
                    return surveyResults.personal == 'No' || surveyResults.hasOwnProperty(field);
                default:
                    return surveyResults.hasOwnProperty(field);
            }
        }).reduce(function (b1, b2) { return b1 && b2; }).value();

    needSurvey = !done;

    if (surveyResults.professional === 'Yes') $(".professionalField").show();
    else $(".professionalField").hide();

    if (surveyResults.personal === 'Yes') $(".personalField").show();
    else $(".personalField").hide();

    $("#nextButton").prop("disabled", needSurvey);
}
