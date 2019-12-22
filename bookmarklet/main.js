const atcoder = function() {
    let testcases = '', toggle = false;
    document.querySelectorAll('.lang>span[style*="display: inline"] .part pre[id]').forEach(function(node) {
        if (toggle) {
            testcases += '--------\n';
        }
        else {
            let header = node.parentNode.querySelector('h3').childNodes[0].textContent;
            testcases += '==== ' + header + ' ====\n';
        }
        toggle = !toggle;
        testcases += node.innerText;
    })
    return testcases;
};

const aoj = function() {
    let testcases = '';
    const pre = document.querySelectorAll('.problemBody pre');
    let h2 = document.querySelectorAll('.problemBody h2');
    h2 = Array.from(h2).slice(h2.length - pre.length)
    for (var i=0; i<pre.length; ++i) {
        testcases += '==== ' + h2[i].innerHTML + ' ====\n';
        testcases += pre[i].innerHTML;
    }
    return testcases;
};

const show = function(text) {
    if (navigator.userAgent.indexOf('Firefox')) {
        alert(text);
    }
    else {
        prompt('Copy this:', text);
    }
};

const main = function() {
    if (location.hostname === 'atcoder.jp') {
        show(atcoder());
    }
    else if (location.hostname === 'onlinejudge.u-aizu.ac.jp') {
        show(aoj());
    }
    else {
        alert('Not supported: ' + location.hostname);
    }
};
