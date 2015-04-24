$(document).ready(function() {
    var options = {
        legend: {
            show: true,
            container: '#legend',
            labelFormatter: function(label, series) {
                var value = series.data[series.data.length - 1][1];
                return "&nbsp;" + label + ": " + value;
            }
        },
        series: {
            shadowSize: 0,
            lines: { show: true, fill: false },
            points: { show: true },
        },
        grid: {
            hoverable: true
        },
        xaxis: { mode: "time" },
        yaxis: { min: 0 }
    };
    var plot = $.plot($("#chart"), [], options);

    $("<div id='tooltip'></div>").css({
        position: "absolute",
        display: "none",
        border: "1px solid #fdd",
        padding: "2px",
        "background-color": "#fee",
        opacity: 0.80
    }).appendTo("body");

    $("#chart").bind("plothover", function(event, pos, item) {
        if (item) {
            var x = new Date(item.datapoint[0]),
            y = item.datapoint[1].toFixed(2);

            $("#tooltip").html(item.series.label + " at " + x.toUTCString() + " = " + y)
            .css({top: item.pageY+5, left: item.pageX+5})
            .fadeIn(200);
        } else {
            $("#tooltip").hide();
        }
    });

    var _data = [];

    var events = new EventSource("/updates" + document.location.search);
    events.onmessage = function(e) {
        var update = JSON.parse(e.data);
        if (update.node != document.title) {
            document.title = update.node;
        }

        for (var i = 0; i < update.m.length; ++i) {
            if (_data.length <= i) {
                var series = {
                    label: update.m[i].n,
                    data: []
                };
                _data.push(series);
            }

            point = [update.t, update.m[i].v];
            _data[i].data.push(point);
        }

        plot.setData(_data);
        plot.setupGrid();
        plot.draw();
    };
});

