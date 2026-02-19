// PACKET STAR
// Must fit in Ethernet IP packet i.e 1500 bytes after compression.
let es;
const tabid = self.crypto.randomUUID().substring(0,8)

function initEventSource() {
  es = new EventSource(window.location.pathname + 'stream' + (window.location.search + '&tabid=' + tabid).replace(/^&/,'?'))

  es.onmessage = (e) => {
    document.getElementsByTagName('body')[0].outerHTML = e.data
  }

  es.onerror = (e) => {
    es.close()
  }
}

initEventSource()

document.addEventListener("visibilitychange", () => {
  if (document.hidden) {
    es.close()
  } else {
    initEventSource()
  }
})

document.addEventListener("mousedown", (e) => {
  const action = e.target.dataset.action
  if (action) {
    fetch(action, { method: "POST" })
  }
})


