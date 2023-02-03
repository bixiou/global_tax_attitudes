#!/bin/bash
for FILE in ./*.pdf; do
  pdfcrop "${FILE}"
  rm "${FILE}"
done

for FILE in ./*.pdf; do
  mv "${FILE}" "${FILE%-crop*}.pdf"
done
