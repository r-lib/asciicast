
// This runs after svg-term is loaded into the global namespace

function get_cast(json, options) {
    var cast = loadcast(json, options);
    for (var i = 0; i < cast.frames.length; i++) {
        cast.frames[i][1].screen.lines =
            simplify_lines(cast.frames[i][1].screen.lines);
    }
    return cast;
}

function simplify_lines(lines) {
    return lines.map(simplify_line);
}

function simplify_line(line) {
    var out = [];
    var str = "";
    var att = line[0][1];
    for (var i = 0; i < line.length; i++) {
        var p = String.fromCodePoint(line[i][0]);
        if (are_arrays_equal(line[i][1], att)) {
            str = str + p;
        } else {
            out.push([str, att]);
            str = p;
            att = line[i][1];
        }
    }
    out.push([str, att]);
    return out;
}

function are_arrays_equal(a1, a2) {
    return JSON.stringify(a1) == JSON.stringify(a2);
}
