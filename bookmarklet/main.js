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

let aoj = function() {
    let testcases = '', isOutput = false, isDone = false;
    const problemBody = document.getElementsByClassName('problemBody')[0];
    Array.from(problemBody.childNodes).filter(function(node) { return node.nodeType === 1 }).reverse().forEach(function(node) {
        if (!isDone) {
            const tagName = node.tagName.toLowerCase();
            switch (tagName) {
                case 'pre':
                    testcases = node.innerHTML + testcases;
                    isOutput = !isOutput
                    break;
                case 'h2':
                    if (testcases) {
                        if (isOutput) {
                            testcases = '--------\n' + testcases;
                        }
                        else {
                            testcases = '==== ' + node.innerHTML + ' ====\n' + testcases;
                        }
                    }
                    break;
                default:
                    if (testcases) {
                        isDone = true;
                    }
            }
        }
    });
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
