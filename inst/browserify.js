
var JSDOM = require('jsdom').JSDOM;

global.dom = new JSDOM('<!doctype html><html><body></body></html>');
global.window = dom.window;
global.document = dom.window.document;
global.navigator = global.window.navigator;

global.svgterm = require('.');
global.loadcast = require('load-asciicast').load;
