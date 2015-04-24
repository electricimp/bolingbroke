$(document).ready(function() {
    var options = {
        legend: {
            show: true,
            position: 'ne'
        },
        series: {
            shadowSize: 0
        },
        xaxis: { mode: "time" },
        yaxis: { min: 0 }
    };
    var plot = $.plot($("#chart"), [], options);

    var _data = [];

    var events = new EventSource("/updates" + document.location.search);
    events.onmessage = function(e) {
        var update = JSON.parse(e.data);

        for (var i = 0; i < update.m.length; ++i) {
            if (_data.length <= i) {
                var series = {
                    label: update.m[i].n,
                    data: [],
                    lines: { show: true, fill: false }
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

