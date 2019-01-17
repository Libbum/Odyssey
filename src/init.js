// Find browser's scrollbar width
var scrollDiv = document.createElement("div");
scrollDiv.className = "scrollbar-measure";
document.body.appendChild(scrollDiv);
var scrollbarWidth = scrollDiv.offsetWidth - scrollDiv.clientWidth;
document.body.removeChild(scrollDiv)

// App entry
var app = Elm.Main.init({
    node: document.getElementById('odyssey'),
    flags: scrollbarWidth
});

// Attach a listener for lazy loading of gallery rows
window.onscroll = function(ev) {
    if ((window.innerHeight + window.pageYOffset + 500) >= document.body.offsetHeight) {
        app.ports.nearBottom.send(true);
    }
};

// Contact form post
document.querySelector('form').addEventListener('submit', function(e) {
    var fd = new FormData(e.target);
    var request = new Request('process.php', {
        method: 'POST',
        body: fd
    });

    fetch(request).then(function(data) {
        return data.text();
    }).then(function(result) {
        document.getElementById('response').innerHTML = result;
        if (~result.indexOf('Thanks')) {
            e.target.reset();
        }
    });

    e.preventDefault();
});


// Ports and fuctions for map control
current = 0;
currentCoords = [-30, -40];
currentCountry = "";
currentLocation = "";
currentTrip = "";

app.ports.initMap.subscribe(function(data) {
    current = data[0];
    requestAnimationFrame(function() {
        requestAnimationFrame(function() {
            switch (current) {
                case 1:
                    drawMap();
                    break;
                case 2:
                    currentCountry = data[1];
                    drawMap(function() {
                        waitForMap("AUS", function() { //Hard code the wait, otherwise tiny countries don't show up.
                            countryView(currentCountry);
                        });
                    });
                    break;
                case 3:
                    currentLocation = data[1];
                    currentCoords = data[2];
                    drawMap(function() {
                        waitForMap(currentLocation, function() {
                            locationView(currentLocation, currentCoords);
                        });
                    });
                    break;
                case 4:
                    currentTrip = data[1];
                    drawMap(function() {
                        waitForMap(currentTrip, function() {
                            tripView(currentTrip);
                        });
                    });
                    break;
                default:
                    current = 1;
                    break;
            }
        });
    });

    return null;
});

app.ports.drawMap.subscribe(function() {
    requestAnimationFrame(function() {
        requestAnimationFrame(function() {
            if (current != 0) {
                proj.rotate(currentCoords).scale(190);
            }
            drawMap(function() {
                switch (current) {
                    case 1:
                        break;
                    case 2:
                        redrawCountry(currentCountry);
                        break;
                    case 3:
                        redrawLocation(currentLocation);
                        break;
                    case 4:
                        redrawTrip(currentTrip);
                        break;
                    default:
                        current = 1;
                        break;
                }
            });
        });
    });
    return null;
});

app.ports.viewAll.subscribe(function() {
    allView();
    return null;
});

app.ports.viewCountry.subscribe(function(countryId) {
    countryView(countryId);
    return null;
});

app.ports.viewLocation.subscribe(function(data) {
    locationView(data[0], data[1]);
    return null;
});

app.ports.showLocation.subscribe(function(data) {
    locationShow(data[0], data[1]);
    return null;
});

app.ports.viewTrip.subscribe(function(tripName) {
    tripView(tripName);
    return null;
});

function allView() {
    flushCountries();
    flushLocations();
    flushTrips();
    current = 1;
    currentCoords = [-30, -40];
    gotoView(currentCoords);
}

function countryView(selected) {
    flushTrips();
    flushLocations();
    d3.selectAll(".iglobe-countries").each(function(d, i) {
        if (d.id == selected) {
            d3.select(this).classed("iglobe-selected", true);
        } else {
            d3.select(this).classed("iglobe-selected", false);
        }
    });
    for (var i = 0; i < countries.length; i++) {
        if (countries[i].id == selected) {
            var cent;
            if (countries[i].properties.name == "Russia") {
                cent = [77, 60]; //Override Russia
            } else {
                var centroid = d3.geo.path().projection(function(d) { return d; }).centroid;
                cent = centroid(countries[i]);
            }
            coords = cent.map(function(v) { return v !== 0 ? -v : v });
            currentCoords = coords;
            gotoView(coords);
            break;
        }
    }
    current = 2;
    currentCountry = selected;
}

d3.selection.prototype.moveUp = function() {
    return this.each(function() {
        this.parentNode.appendChild(this);
    });
};

function locationView(loc, coords) {
    flushCountries();
    flushTrips();
    locationShow(loc, coords);
    current = 3;
    currentLocation = loc;
    currentCoords = coords;
}

function locationShow(loc, coords) {
    flushLocations();
    if (loc.length > 0) {
        d3.selectAll("#"+loc).classed("iglobe-highlight", true);
        d3.selectAll("#"+loc).moveUp();
    }
    if (coords.length == 2) {
        gotoView(coords);
    }
    return false;
}

function tripView(selected) {
    flushCountries();
    flushLocations();
    d3.selectAll(".iglobe-route").each(function(d, i) {
        if (d.properties.name == selected) {
            d3.select(this).attr("visibility", "visible");
            coords = getRotation(d.geometry.coordinates);
            gotoView(coords);
            currentCoords = coords;
        } else {
            d3.select(this).attr("visibility", "hidden");
        }
    });
    current = 4;
    currentTrip = selected;
}

function redrawCountry(selected) {
    waitForMap(selected, function() {
        d3.selectAll(".iglobe-countries").each(function(d, i) {
            if (d.id == selected) {
                d3.select(this).classed("iglobe-selected", true);
            }
        });
    });
}

function redrawLocation(selected) {
    waitForMap(selected, function() {
        d3.selectAll("#"+selected).classed("iglobe-highlight", true);
        d3.selectAll("#"+selected).moveUp();
    });
}

function redrawTrip(selected) {
    waitForMap(selected, function() {
        d3.selectAll(".iglobe-route").each(function(d, i) {
            if (d.properties.name == selected) {
                d3.select(this).attr("visibility", "visible");
            }
        });
    });
}

function waitForMap(selected, callBack){
    window.setTimeout(function(){
        var element = document.getElementById(selected);
        if(element){
            callBack();
        }else{
            waitForMap(selected, callBack);
        }
    },10)
}

function flushCountries() {
    d3.selectAll(".iglobe-countries").each(function() {
        d3.select(this).classed("iglobe-selected", false);
    });
}

function flushLocations() {
    d3.selectAll("#cities").selectAll("use").remove();
    d3.selectAll(".iglobe-cities").each(function(d, i) {
        d3.select(this).classed("iglobe-highlight", false);
    });
}

function flushTrips() {
    d3.selectAll(".iglobe-route").each(function() {
        d3.select(this).attr("visibility", "hidden");
    });
}

function getRotation(coords) {
    var lat = 0,
        long = 0,
        len = coords.length - 1;
    do {
        lat += coords[len][0];
        long += coords[len][1];
    } while (len--);
    lat /= coords.length;
    long /= coords.length;
    return [-lat, -long];
}

function gotoView(coords) {
    var interp = sphereRotate();
    d3.transition().delay(500).duration(2000)
        .tween("rotate", function() {
            var sc = d3.interpolate(proj.scale(), 190); //width / 2 - 10 = 190
            //If statement stops us from transitioning to ourself and NaNing out.
            if (interp.source(proj.rotate()).target(coords).distance() > 0) {
                return function(i) {
                    proj.rotate(interp(i)).scale(sc(i));
                    m.scale(sc(i));
                    d3.select("#map").selectAll("path").attr("d", d3.geo.path().projection(proj));
                };
            } else {
                return false;
            }
        });
}

function sphereRotate() {
    var x0, y0, cy0, sy0, kx0, ky0,
        x1, y1, cy1, sy1, kx1, ky1,
        d, k, d3_radians = Math.PI / 180;

    function interpolate(t) {
        var B = Math.sin(t *= d) * k,
            A = Math.sin(d - t) * k,
            x = A * kx0 + B * kx1,
            y = A * ky0 + B * ky1,
            z = A * sy0 + B * sy1;
        return [Math.atan2(y, x) / d3_radians, Math.atan2(z, Math.sqrt(x * x + y * y)) / d3_radians];
    }

    interpolate.distance = function() {
        if (d === null) k = 1 / Math.sin(d = Math.acos(Math.max(-1, Math.min(1, sy0 * sy1 + cy0 * cy1 * Math.cos(x1 - x0)))));
        return d;
    };

    interpolate.source = function(_) {
        var cx0 = Math.cos(x0 = _[0] * d3_radians),
            sx0 = Math.sin(x0);
        cy0 = Math.cos(y0 = _[1] * d3_radians);
        sy0 = Math.sin(y0);
        kx0 = cy0 * cx0;
        ky0 = cy0 * sx0;
        d = null;
        return interpolate;
    };

    interpolate.target = function(_) {
        var cx1 = Math.cos(x1 = _[0] * d3_radians),
            sx1 = Math.sin(x1);
        cy1 = Math.cos(y1 = _[1] * d3_radians);
        sy1 = Math.sin(y1);
        kx1 = cy1 * cx1;
        ky1 = cy1 * sx1;
        d = null;
        return interpolate;
    };

    return interpolate;
}

// Map
proj = d3.geo.orthographic().precision(0.5).clipAngle(90).clipExtent([[-1, -1],[401, 401]]).translate([200, 200]).scale(190).rotate([-40, -30]);

var c = -1,
    rr = d3.dispatch("world"),
    d = Math.PI / 180,
    h = 180 / Math.PI;



function t(t, n, e) {
    var a = t.translate(), //From d3
        o = Math.atan2(n[1] - a[1], n[0] - a[0]) - Math.atan2(e[1] - a[1], e[0] - a[0]);
    return [Math.cos(o / 2), 0, 0, Math.sin(o / 2)];
}

function n(t, n) {
    var e = t.invert(n);
    return e && isFinite(e[0]) && isFinite(e[1]) && i(e);
}

function e(t) {
    var n = 0.5 * t[0] * d, p = 0.5 * t[1] * d, a = 0.5 * t[2] * d,
        o = Math.sin(n), r = Math.cos(n), c = Math.sin(p),
        i = Math.cos(p), s = Math.sin(a), l = Math.cos(a);
    return [r * i * l + o * c * s, o * i * l - r * c * s, r * c * l + o * i * s, r * i * s - o * c * l];
}

function a(t, n) {
    var x = t[0], a = t[1], o = t[2], r = t[3],
        c = n[0], i = n[1], s = n[2], l = n[3];
    return [x * c - a * i - o * s - r * l, x * i + a * c + o * l - r * s, x * s - a * l + o * c + r * i, x * l + a * s - o * i + r * c];
}

function o(t, n) {
    if (t && n) {
        var e = l(t, n),
            a = Math.sqrt(s(e, e)),
            o = 0.5 * Math.acos(Math.max(-1, Math.min(1, s(t, n)))),
            r = Math.sin(o) / a;
        return a && [Math.cos(o), e[2] * r, -e[1] * r, e[0] * r];
    }
}

function r(t, n) {
    var e = Math.max(-1, Math.min(1, s(t, n))),
        a = 0 > e ? -1 : 1,
        o = Math.acos(a * e),
        r = Math.sin(o);
    return r ? function(e) {
        var c = a * Math.sin((1 - e) * o) / r,
            i = Math.sin(e * o) / r;
        return [t[0] * c + n[0] * i, t[1] * c + n[1] * i, t[2] * c + n[2] * i, t[3] * c + n[3] * i];
    } : function() {
        return t;
    };
}

function cc(t) {
    return [Math.atan2(2 * (t[0] * t[1] + t[2] * t[3]), 1 - 2 * (t[1] * t[1] + t[2] * t[2])) * h, Math.asin(Math.max(-1, Math.min(1, 2 * (t[0] * t[2] - t[3] * t[1])))) * h, Math.atan2(2 * (t[0] * t[3] + t[1] * t[2]), 1 - 2 * (t[2] * t[2] + t[3] * t[3])) * h];
}

function i(t) {
    var n = t[0] * d,
        e = t[1] * d,
        a = Math.cos(e);
    return [a * Math.cos(n), a * Math.sin(n), Math.sin(e)];
}

function s(t, n) {
    var a = t.length - 1, o = 0;
    do { o += t[a] * n[a]; } while (a--);
    return o;
}

function l(t, n) {
    return [t[1] * n[2] - t[2] * n[1], t[2] * n[0] - t[0] * n[2], t[0] * n[1] - t[1] * n[0]];
}

function u(t) {
    for (var n = 0, e = arguments.length, a = []; ++n < e;) a.push(arguments[n]);
    var o = d3.dispatch.apply(null, a);
    return o.of = function(n, e) {
        return function(a) {
            var r;
            try {
                r = a.sourceEvent = d3.event;
                a.target = t;
                d3.event = a;
                o[a.type].apply(n, e);
            } finally {
                d3.event = r;
            }
        };
    }, o;
}

d3.geo.zoom = function() {
    function s(t) { v++ || t({ type: "zoomstart" }); }
    function l(t) { t({ type: "zoom" }); }
    function d(t) { --v || t({ type: "zoomend" }); }

    var h, f, p, v = 0,
        g = u(m, "zoomstart", "zoom", "zoomend");
    m.on("zoomstart", function() {
        var r = d3.mouse(this),
            i = e(h.rotate()),
            u = n(h, r);
        if (u) p = u;
        M.call(m, "zoom", function() {
            h.scale(z.k = d3.event.scale);
            var e = d3.mouse(this),
                s = o(p, n(h, e));
            h.rotate(z.r = cc(i = s ? a(i, s) : a(t(h, r, e), i)));
            r = e;
            l(g.of(this, arguments));
        });
        s(g.of(this, arguments));
    }).on("zoomend", function() {
        M.call(m, "zoom", null);
        d(g.of(this, arguments));
    });
    M = m.on;
    z = { r: [0, 0, 0], k: 1 };
    return m.rotateTo = function(t) {
        var n = o(i(t), i([-z.r[0], -z.r[1]]));
        return cc(a(e(z.r), n));//
    }, m.projection = function(t) {
        return arguments.length ? (h = t, z = {
            r: h.rotate(),
            k: h.scale()
        }, m.scale(z.k)) : h;
    }, m.duration = function(t) {
        return arguments.length ? (f = t, m) : f;
    }, m.event = function(t) {
        t.each(function() {
            var t = d3.select(this),
                n = g.of(this, arguments),
                a = z,
                o = d3.transition(t);
            if (o !== t) {
                o.each("start.zoom", function() {
                    if (this.__chart__) z = this.__chart__;
                    h.rotate(z.r).scale(z.k);
                    s(n);
                }).tween("zoom:zoom", function() {
                    var t = m.size()[0],
                        i = r(e(z.r), e(a.r)),
                        s = d3.geo.distance(z.r, a.r),
                        u = d3.interpolateZoom([0, 0, t / z.k], [s, 0, t / a.k]);
                    return f && o.duration(f(0.001 * u.duration)),
                        function(e) {
                            var a = u(e);
                            this.__chart__ = z = { r: cc(i(a[0] / s)), k: t / a[2] };//
                            h.rotate(z.r).scale(z.k);
                            m.scale(z.k);
                            l(n);
                        };
                }).each("end.zoom", function() { d(n); });
                try {
                    o.each("interrupt.zoom", function() { d(n); });
                } catch (i) {}
            } else {
                this.__chart__ = z;
                s(n);
                l(n);
                d(n);
            }
        });
    }, d3.rebind(m, g, "on");
};

function grab(t, n, e) {
    var a = n.projection(); //From d3
    t.append("path").datum(d3.geo.graticule()).attr("class", "iglobe-graticule").attr("d", n);
    t.append("path").datum({ type: "Sphere" }).attr("class", "iglobe-foreground").attr("d", n).on("mousedown.grab", function() {
        var n;
        d3.transition(); //Kill any current globe movements.
        if (e) n = t.insert("path", ".iglobe-foreground").datum({ type: "Point", coordinates: a.invert(d3.mouse(this)) })
            .attr("class", "iglobe-point").attr("d", o);
        var o = d3.select(this).classed("zooming", !0),
            rr = d3.select(window).on("mouseup.grab", function() {
                o.classed("zooming", !1);
                rr.on("mouseup.grab", null);
                if (e) n.remove();
            });
    });
}

function drawMap(callback) {
    m = d3.behavior.zoom();

    d3.selectAll("#map").data([proj]).append("svg").attr("preserveAspectRatio", "xMinYMin meet").attr("viewBox", "0 0 400 400").each(function(p) {
        var e = d3.geo.path().projection(p),
            a = d3.select(this).call(grab, e, !0);
        a.selectAll(".iglobe-foreground").call(d3.geo.zoom().projection(p).scaleExtent([0.7 * p.scale(), 10 * p.scale()]).on("zoom.redraw", function() {
            if (d3.event.sourceEvent.preventDefault) d3.event.sourceEvent.preventDefault();
            a.selectAll("path").attr("d", e);
        })).on("dblclick.zoom", function() {gotoView([-40, -30]);});
        rr.on("world." + c++, function() {
            a.selectAll("path").attr("d", e);
        });
    });

    d3.json("/assets/world.json", function(t, n) {
        var svg = d3.selectAll("svg");
        svg.insert("path", ".iglobe-graticule").datum({ type: "Sphere"}).attr("class", "iglobe-ocean");
        countries = topojson.feature(n, n.objects.countries).features;
        cities = topojson.feature(n, n.objects.cities).features;
        svg.insert("g", ".iglobe-foreground").attr("id", "countries");
        d3.selectAll("#countries").selectAll("path").data(countries.filter(function(d) { return d.geometry.type !== 'Point'; }))
            .enter().append("path").attr("class", "iglobe-countries").attr("id", function(d, i) { return d.id; });
        svg.insert("g", ".iglobe-foreground").attr("id", "cities");
        d3.selectAll("#cities").selectAll("path").data(cities).enter().append("path").attr("class", "iglobe-cities")
            .attr("id", function(d, i) { return d.properties.name.replace(/ /g,"_"); }).selectAll("LineString").attr("class", "iglobe-route");
        svg.insert("g", "#cities").attr("id", "routes");
        d3.selectAll("#routes").selectAll("path").data(topojson.feature(n, n.objects.trips).features).enter()
            .append("path").attr("id", function(d) { return d.properties.name; }).attr("class", "iglobe-route")
            .attr("visibility", function(d) { return "hidden"; });
        rr.world();
    });

    callback();
}
